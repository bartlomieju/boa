//! This module implements the global `Array` object.
//!
//! The JavaScript `Array` class is a global object that is used in the construction of arrays; which are high-level, list-like objects.
//!
//! More information:
//!  - [ECMAScript reference][spec]
//!  - [MDN documentation][mdn]
//!
//! [spec]: https://tc39.es/ecma262/#sec-array-objects
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array

pub mod array_iterator;
#[cfg(test)]
mod tests;

use crate::{
    builtins::array::array_iterator::{ArrayIterationKind, ArrayIterator},
    builtins::BuiltIn,
    builtins::Number,
    object::{ConstructorBuilder, FunctionBuilder, GcObject, ObjectData, PROTOTYPE},
    property::{Attribute, PropertyDescriptor},
    symbol::WellKnownSymbols,
    value::{IntegerOrInfinity, Value},
    BoaProfiler, Context, JsString, Result,
};
use std::cmp::{max, min};

/// JavaScript `Array` built-in implementation.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Array;

impl BuiltIn for Array {
    const NAME: &'static str = "Array";

    fn attribute() -> Attribute {
        Attribute::WRITABLE | Attribute::NON_ENUMERABLE | Attribute::CONFIGURABLE
    }

    fn init(context: &mut Context) -> (&'static str, Value, Attribute) {
        let _timer = BoaProfiler::global().start_event(Self::NAME, "init");

        let symbol_iterator = WellKnownSymbols::iterator();

        let get_species = FunctionBuilder::native(context, Self::get_species)
            .name("get [Symbol.species]")
            .constructable(false)
            .build();

        let values_function = FunctionBuilder::native(context, Self::values)
            .name("values")
            .length(0)
            .constructable(false)
            .build();

        let array = ConstructorBuilder::with_standard_object(
            context,
            Self::constructor,
            context.standard_objects().array_object().clone(),
        )
        .name(Self::NAME)
        .length(Self::LENGTH)
        .static_accessor(
            WellKnownSymbols::species(),
            Some(get_species),
            None,
            Attribute::CONFIGURABLE,
        )
        .property(
            "length",
            0,
            Attribute::WRITABLE | Attribute::NON_ENUMERABLE | Attribute::PERMANENT,
        )
        .property(
            "values",
            values_function.clone(),
            Attribute::WRITABLE | Attribute::NON_ENUMERABLE | Attribute::CONFIGURABLE,
        )
        .property(
            symbol_iterator,
            values_function,
            Attribute::WRITABLE | Attribute::NON_ENUMERABLE | Attribute::CONFIGURABLE,
        )
        .method(Self::concat, "concat", 1)
        .method(Self::push, "push", 1)
        .method(Self::index_of, "indexOf", 1)
        .method(Self::last_index_of, "lastIndexOf", 1)
        .method(Self::includes_value, "includes", 1)
        .method(Self::map, "map", 1)
        .method(Self::fill, "fill", 1)
        .method(Self::for_each, "forEach", 1)
        .method(Self::filter, "filter", 1)
        .method(Self::pop, "pop", 0)
        .method(Self::join, "join", 1)
        .method(Self::to_string, "toString", 0)
        .method(Self::reverse, "reverse", 0)
        .method(Self::shift, "shift", 0)
        .method(Self::unshift, "unshift", 1)
        .method(Self::every, "every", 1)
        .method(Self::find, "find", 1)
        .method(Self::find_index, "findIndex", 1)
        .method(Self::flat, "flat", 0)
        .method(Self::flat_map, "flatMap", 1)
        .method(Self::slice, "slice", 2)
        .method(Self::some, "some", 2)
        .method(Self::reduce, "reduce", 2)
        .method(Self::reduce_right, "reduceRight", 2)
        .method(Self::keys, "keys", 0)
        .method(Self::entries, "entries", 0)
        .method(Self::copy_within, "copyWithin", 3)
        // Static Methods
        .static_method(Self::is_array, "isArray", 1)
        .static_method(Self::of, "of", 0)
        .build();

        (Self::NAME, array.into(), Self::attribute())
    }
}

impl Array {
    const LENGTH: usize = 1;

    fn constructor(new_target: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // If NewTarget is undefined, let newTarget be the active function object; else let newTarget be NewTarget.
        // 2. Let proto be ? GetPrototypeFromConstructor(newTarget, "%Array.prototype%").
        let prototype = new_target
            .as_object()
            .and_then(|obj| {
                obj.__get__(&PROTOTYPE.into(), obj.clone().into(), context)
                    .map(|o| o.as_object())
                    .transpose()
            })
            .transpose()?
            .unwrap_or_else(|| context.standard_objects().array_object().prototype());

        // 3. Let numberOfArgs be the number of elements in values.
        let number_of_args = args.len();

        // 4. If numberOfArgs = 0, then
        if number_of_args == 0 {
            // 4.a. Return ! ArrayCreate(0, proto).
            Ok(Array::array_create(0, Some(prototype), context)
                .unwrap()
                .into())
        // 5. Else if numberOfArgs = 1, then
        } else if number_of_args == 1 {
            // a. Let len be values[0].
            let len = &args[0];
            // b. Let array be ! ArrayCreate(0, proto).
            let array = Array::array_create(0, Some(prototype), context).unwrap();
            // c. If Type(len) is not Number, then
            let int_len = if !len.is_number() {
                // i. Perform ! CreateDataPropertyOrThrow(array, "0", len).
                array
                    .create_data_property_or_throw(0, len, context)
                    .unwrap();
                // ii. Let intLen be 1𝔽.
                1
            // d. Else,
            } else {
                // i. Let intLen be ! ToUint32(len).
                let int_len = len.to_u32(context).unwrap();
                // ii. If SameValueZero(intLen, len) is false, throw a RangeError exception.
                if !Value::same_value_zero(&int_len.into(), len) {
                    return Err(context.construct_range_error("invalid array length"));
                }
                int_len
            };
            // e. Perform ! Set(array, "length", intLen, true).
            array.set("length", int_len, true, context).unwrap();
            // f. Return array.
            Ok(array.into())
        // 6. Else,
        } else {
            // 6.a. Assert: numberOfArgs ≥ 2.
            debug_assert!(number_of_args >= 2);

            // b. Let array be ? ArrayCreate(numberOfArgs, proto).
            let array = Array::array_create(number_of_args, Some(prototype), context)?;
            // c. Let k be 0.
            // d. Repeat, while k < numberOfArgs,
            for (i, item) in args.iter().cloned().enumerate() {
                // i. Let Pk be ! ToString(𝔽(k)).
                // ii. Let itemK be values[k].
                // iii. Perform ! CreateDataPropertyOrThrow(array, Pk, itemK).
                array
                    .create_data_property_or_throw(i, item, context)
                    .unwrap();
                // iv. Set k to k + 1.
            }
            // e. Assert: The mathematical value of array's "length" property is numberOfArgs.
            // f. Return array.
            Ok(array.into())
        }
    }

    /// Utility for constructing `Array` objects.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-arraycreate
    pub(crate) fn array_create(
        length: usize,
        prototype: Option<GcObject>,
        context: &mut Context,
    ) -> Result<GcObject> {
        // 1. If length > 2^32 - 1, throw a RangeError exception.
        if length > 2usize.pow(32) - 1 {
            return Err(context.construct_range_error("array exceeded max size"));
        }
        // 7. Return A.
        // 2. If proto is not present, set proto to %Array.prototype%.
        // 3. Let A be ! MakeBasicObject(« [[Prototype]], [[Extensible]] »).
        // 4. Set A.[[Prototype]] to proto.
        // 5. Set A.[[DefineOwnProperty]] as specified in 10.4.2.1.
        let prototype = match prototype {
            Some(prototype) => prototype,
            None => context.standard_objects().array_object().prototype(),
        };
        let array = context.construct_object();

        array.set_prototype_instance(prototype.into());
        // This value is used by console.log and other routines to match Object type
        // to its Javascript Identifier (global constructor method name)
        array.borrow_mut().data = ObjectData::Array;

        // 6. Perform ! OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor { [[Value]]: 𝔽(length), [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).

        array.ordinary_define_own_property(
            "length".into(),
            PropertyDescriptor::builder()
                .value(length as f64)
                .writable(true)
                .enumerable(false)
                .configurable(false)
                .build(),
        );

        Ok(array)
    }

    /// Creates a new `Array` instance.
    pub(crate) fn new_array(context: &Context) -> Value {
        let array = Value::new_object(context);
        array.set_data(ObjectData::Array);
        array
            .as_object()
            .expect("'array' should be an object")
            .set_prototype_instance(context.standard_objects().array_object().prototype().into());
        array.set_property(
            "length",
            PropertyDescriptor::builder()
                .value(0)
                .writable(true)
                .enumerable(false)
                .configurable(false)
                .build(),
        );
        array
    }

    /// Utility function for creating array objects.
    ///
    /// `array_obj` can be any array with prototype already set (it will be wiped and
    /// recreated from `array_contents`)
    pub(crate) fn construct_array(
        array_obj: &Value,
        array_contents: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        let array_obj_ptr = array_obj.clone();

        // Wipe existing contents of the array object
        let orig_length = array_obj.get_field("length", context)?.to_length(context)?;
        for n in 0..orig_length {
            array_obj_ptr.remove_property(n);
        }

        // Create length
        array_obj_ptr.set_property(
            "length".to_string(),
            PropertyDescriptor::builder()
                .value(array_contents.len())
                .writable(true)
                .enumerable(false)
                .configurable(false)
                .build(),
        );

        for (n, value) in array_contents.iter().enumerate() {
            array_obj_ptr.set_property(
                n,
                PropertyDescriptor::builder()
                    .value(value)
                    .configurable(true)
                    .enumerable(true)
                    .writable(true),
            );
        }
        Ok(array_obj_ptr)
    }

    /// Utility function for concatenating array objects.
    ///
    /// Returns a Boolean valued property that if `true` indicates that
    /// an object should be flattened to its array elements
    /// by `Array.prototype.concat`.
    fn is_concat_spreadable(this: &Value, context: &mut Context) -> Result<bool> {
        // 1. If Type(O) is not Object, return false.
        if !this.is_object() {
            return Ok(false);
        }
        // 2. Let spreadable be ? Get(O, @@isConcatSpreadable).
        let spreadable = this.get_field(WellKnownSymbols::is_concat_spreadable(), context)?;

        // 3. If spreadable is not undefined, return ! ToBoolean(spreadable).
        if !spreadable.is_undefined() {
            return Ok(spreadable.to_boolean());
        }
        // 4. Return ? IsArray(O).
        match this.as_object() {
            Some(obj) => Ok(obj.is_array()),
            _ => Ok(false),
        }
    }

    /// `get Array [ @@species ]`
    ///
    /// The `Array [ @@species ]` accessor property returns the Array constructor.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-get-array-@@species
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/@@species
    fn get_species(this: &Value, _: &[Value], _: &mut Context) -> Result<Value> {
        // 1. Return the this value.
        Ok(this.clone())
    }

    /// Utility function used to specify the creation of a new Array object using a constructor
    /// function that is derived from original_array.
    ///
    /// see: <https://tc39.es/ecma262/#sec-arrayspeciescreate>
    pub(crate) fn array_species_create(
        original_array: &GcObject,
        length: usize,
        context: &mut Context,
    ) -> Result<GcObject> {
        // 1. Let isArray be ? IsArray(originalArray).
        // 2. If isArray is false, return ? ArrayCreate(length).
        if !original_array.is_array() {
            return Self::array_create(length, None, context);
        }
        // 3. Let C be ? Get(originalArray, "constructor").
        let c = original_array.get("constructor", context)?;

        // 4. If IsConstructor(C) is true, then
        //     a. Let thisRealm be the current Realm Record.
        //     b. Let realmC be ? GetFunctionRealm(C).
        //     c. If thisRealm and realmC are not the same Realm Record, then
        //         i. If SameValue(C, realmC.[[Intrinsics]].[[%Array%]]) is true, set C to undefined.
        // TODO: Step 4 is ignored, as there are no different realms for now

        // 5. If Type(C) is Object, then
        let c = if let Some(c) = c.as_object() {
            // 5.a. Set C to ? Get(C, @@species).
            let c = c.get(WellKnownSymbols::species(), context)?;
            // 5.b. If C is null, set C to undefined.
            if c.is_null_or_undefined() {
                Value::undefined()
            } else {
                c
            }
        } else {
            c
        };

        // 6. If C is undefined, return ? ArrayCreate(length).
        if c.is_undefined() {
            return Self::array_create(length, None, context);
        }

        // 7. If IsConstructor(C) is false, throw a TypeError exception.
        if let Some(c) = c.as_object() {
            if !c.is_constructable() {
                return Err(context.construct_type_error("Symbol.species must be a constructor"));
            }
            // 8. Return ? Construct(C, « 𝔽(length) »).
            Ok(
                c.construct(&[Value::from(length)], &c.clone().into(), context)?
                    .as_object()
                    .unwrap(),
            )
        } else {
            Err(context.construct_type_error("Symbol.species must be a constructor"))
        }
    }

    /// Utility function which takes an existing array object and puts additional
    /// values on the end, correctly rewriting the length
    pub(crate) fn add_to_array_object(
        array_ptr: &Value,
        add_values: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        let orig_length = array_ptr.get_field("length", context)?.to_length(context)?;

        for (n, value) in add_values.iter().enumerate() {
            let new_index = orig_length.wrapping_add(n);
            array_ptr.set_property(
                new_index,
                PropertyDescriptor::builder()
                    .value(value)
                    .configurable(true)
                    .enumerable(true)
                    .writable(true),
            );
        }

        array_ptr.set_field(
            "length",
            Value::from(orig_length.wrapping_add(add_values.len())),
            false,
            context,
        )?;

        Ok(array_ptr.clone())
    }

    /// `Array.isArray( arg )`
    ///
    /// The isArray function takes one argument arg, and returns the Boolean value true
    /// if the argument is an object whose class internal property is "Array"; otherwise it returns false.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.isarray
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/isArray
    pub(crate) fn is_array(_: &Value, args: &[Value], _: &mut Context) -> Result<Value> {
        match args.get(0).and_then(|x| x.as_object()) {
            Some(object) => Ok(Value::from(object.borrow().is_array())),
            None => Ok(Value::from(false)),
        }
    }

    /// `Array.of(...items)`
    ///
    /// The Array.of method creates a new Array instance from a variable number of arguments,
    /// regardless of the number or type of arguments.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.of
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/of
    pub(crate) fn of(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let len be the number of elements in items.
        // 2. Let lenNumber be 𝔽(len).
        let len = args.len();

        // 3. Let C be the this value.
        // 4. If IsConstructor(C) is true, then
        //     a. Let A be ? Construct(C, « lenNumber »).
        // 5. Else,
        //     a. Let A be ? ArrayCreate(len).
        let a = match this.as_object() {
            Some(object) if object.is_constructable() => object
                .construct(&[len.into()], this, context)?
                .as_object()
                .unwrap(),
            _ => Array::array_create(len, None, context)?,
        };

        // 6. Let k be 0.
        // 7. Repeat, while k < len,
        for (k, value) in args.iter().enumerate() {
            // a. Let kValue be items[k].
            // b. Let Pk be ! ToString(𝔽(k)).
            // c. Perform ? CreateDataPropertyOrThrow(A, Pk, kValue).
            a.create_data_property_or_throw(k, value, context)?;
            // d. Set k to k + 1.
        }

        // 8. Perform ? Set(A, "length", lenNumber, true).
        a.set("length", len, true, context)?;

        // 9. Return A.
        Ok(a.into())
    }

    /// `Array.prototype.concat(...arguments)`
    ///
    /// When the concat method is called with zero or more arguments, it returns an
    /// array containing the array elements of the object followed by the array
    /// elements of each argument in order.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.concat
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat
    pub(crate) fn concat(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let obj = this.to_object(context)?;
        // 2. Let A be ? ArraySpeciesCreate(O, 0).
        let arr = Self::array_species_create(&obj, 0, context)?;
        // 3. Let n be 0.
        let mut n = 0;
        // 4. Prepend O to items.
        // 5. For each element E of items, do
        for item in [Value::from(obj)].iter().chain(args.iter()) {
            // a. Let spreadable be ? IsConcatSpreadable(E).
            let spreadable = Self::is_concat_spreadable(item, context)?;
            // b. If spreadable is true, then
            if spreadable {
                // item is guaranteed to be an object since is_concat_spreadable checks it,
                // so we can call `.unwrap()`
                let item = item.as_object().unwrap();
                // i. Let k be 0.
                // ii. Let len be ? LengthOfArrayLike(E).
                let len = item.length_of_array_like(context)?;
                // iii. If n + len > 2^53 - 1, throw a TypeError exception.
                if n + len > Number::MAX_SAFE_INTEGER as usize {
                    return context.throw_type_error(
                        "length + number of arguments exceeds the max safe integer limit",
                    );
                }
                // iv. Repeat, while k < len,
                for k in 0..len {
                    // 1. Let P be ! ToString(𝔽(k)).
                    // 2. Let exists be ? HasProperty(E, P).
                    let exists = item.has_property(k, context)?;
                    // 3. If exists is true, then
                    if exists {
                        // a. Let subElement be ? Get(E, P).
                        let sub_element = item.get(k, context)?;
                        // b. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), subElement).
                        arr.create_data_property_or_throw(n, sub_element, context)?;
                    }
                    // 4. Set n to n + 1.
                    n += 1;
                    // 5. Set k to k + 1.
                }
            }
            // c. Else,
            else {
                // i. NOTE: E is added as a single item rather than spread.
                // ii. If n ≥ 2^53 - 1, throw a TypeError exception.
                if n >= Number::MAX_SAFE_INTEGER as usize {
                    return context.throw_type_error("length exceeds the max safe integer limit");
                }
                // iii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), E).
                arr.create_data_property_or_throw(n, item, context)?;
                // iv. Set n to n + 1.
                n += 1
            }
        }
        // 6. Perform ? Set(A, "length", 𝔽(n), true).
        arr.set("length", n, true, context)?;

        // 7. Return A.
        Ok(Value::from(arr))
    }

    /// `Array.prototype.push( ...items )`
    ///
    /// The arguments are appended to the end of the array, in the order in which
    /// they appear. The new length of the array is returned as the result of the
    /// call.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.push
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push
    pub(crate) fn push(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let mut len = o.length_of_array_like(context)? as u64;
        // 3. Let argCount be the number of elements in items.
        let arg_count = args.len() as u64;
        // 4. If len + argCount > 2^53 - 1, throw a TypeError exception.
        if len + arg_count > 2u64.pow(53) - 1 {
            return context.throw_type_error(
                "the length + the number of arguments exceed the maximum safe integer limit",
            );
        }
        // 5. For each element E of items, do
        for element in args.iter().cloned() {
            // a. Perform ? Set(O, ! ToString(𝔽(len)), E, true).
            o.set(len, element, true, context)?;
            // b. Set len to len + 1.
            len += 1;
        }
        // 6. Perform ? Set(O, "length", 𝔽(len), true).
        o.set("length", len, true, context)?;
        // 7. Return 𝔽(len).
        Ok(len.into())
    }

    /// `Array.prototype.pop()`
    ///
    /// The last element of the array is removed from the array and returned.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.pop
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop
    pub(crate) fn pop(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If len = 0, then
        if len == 0 {
            // a. Perform ? Set(O, "length", +0𝔽, true).
            o.set("length", 0, true, context)?;
            // b. Return undefined.
            Ok(Value::undefined())
        // 4. Else,
        } else {
            // a. Assert: len > 0.
            // b. Let newLen be 𝔽(len - 1).
            let new_len = len - 1;
            // c. Let index be ! ToString(newLen).
            let index = new_len;
            // d. Let element be ? Get(O, index).
            let element = o.get(index, context)?;
            // e. Perform ? DeletePropertyOrThrow(O, index).
            o.delete_property_or_throw(index, context)?;
            // f. Perform ? Set(O, "length", newLen, true).
            o.set("length", new_len, true, context)?;
            // g. Return element.
            Ok(element)
        }
    }

    /// `Array.prototype.forEach( callbackFn [ , thisArg ] )`
    ///
    /// This method executes the provided callback function for each element in the array.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.foreach
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach
    pub(crate) fn for_each(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = if let Some(arg) = args
            .get(0)
            .and_then(Value::as_object)
            .filter(GcObject::is_callable)
        {
            arg
        } else {
            return context.throw_type_error("Array.prototype.forEach: invalid callback function");
        };
        // 4. Let k be 0.
        // 5. Repeat, while k < len,
        for k in 0..len {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Let kPresent be ? HasProperty(O, Pk).
            let present = o.has_property(pk, context)?;
            // c. If kPresent is true, then
            if present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(pk, context)?;
                // ii. Perform ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
                let this_arg = args.get(1).cloned().unwrap_or_else(Value::undefined);
                callback.call(&this_arg, &[k_value, k.into(), o.clone().into()], context)?;
            }
            // d. Set k to k + 1.
        }
        // 6. Return undefined.
        Ok(Value::undefined())
    }

    /// `Array.prototype.join( separator )`
    ///
    /// The elements of the array are converted to Strings, and these Strings are
    /// then concatenated, separated by occurrences of the separator. If no
    /// separator is provided, a single comma is used as the separator.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.join
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join
    pub(crate) fn join(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If separator is undefined, let sep be the single-element String ",".
        // 4. Else, let sep be ? ToString(separator).
        let separator = if let Some(separator) = args.get(0) {
            separator.to_string(context)?
        } else {
            JsString::new(",")
        };

        // 5. Let R be the empty String.
        let mut r = String::new();
        // 6. Let k be 0.
        // 7. Repeat, while k < len,
        for k in 0..len {
            // a. If k > 0, set R to the string-concatenation of R and sep.
            if k > 0 {
                r.push_str(&separator);
            }
            // b. Let element be ? Get(O, ! ToString(𝔽(k))).
            let element = o.get(k, context)?;
            // c. If element is undefined or null, let next be the empty String; otherwise, let next be ? ToString(element).
            let next = if element.is_null_or_undefined() {
                JsString::new("")
            } else {
                element.to_string(context)?
            };
            // d. Set R to the string-concatenation of R and next.
            r.push_str(&next);
            // e. Set k to k + 1.
        }
        // 8. Return R.
        Ok(r.into())
    }

    /// `Array.prototype.toString( separator )`
    ///
    /// The toString function is intentionally generic; it does not require that
    /// its this value be an Array object. Therefore it can be transferred to
    /// other kinds of objects for use as a method.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.tostring
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn to_string(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let array be ? ToObject(this value).
        let array = this.to_object(context)?;
        // 2. Let func be ? Get(array, "join").
        let func = array.get("join", context)?;
        // 3. If IsCallable(func) is false, set func to the intrinsic function %Object.prototype.toString%.
        // 4. Return ? Call(func, array).
        if let Some(func) = func.as_object().filter(GcObject::is_callable) {
            func.call(&array.into(), &[], context)
        } else {
            crate::builtins::object::Object::to_string(&array.into(), &[], context)
        }
    }

    /// `Array.prototype.reverse()`
    ///
    /// The elements of the array are rearranged so as to reverse their order.
    /// The object is returned as the result of the call.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.reverse
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse
    #[allow(clippy::else_if_without_else)]
    pub(crate) fn reverse(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. Let middle be floor(len / 2).
        let middle = len / 2;
        // 4. Let lower be 0.
        let mut lower = 0;
        // 5. Repeat, while lower ≠ middle,
        while lower != middle {
            // a. Let upper be len - lower - 1.
            let upper = len - lower - 1;
            // Skiped: b. Let upperP be ! ToString(𝔽(upper)).
            // Skiped: c. Let lowerP be ! ToString(𝔽(lower)).
            // d. Let lowerExists be ? HasProperty(O, lowerP).
            let lower_exists = o.has_property(lower, context)?;
            // e. If lowerExists is true, then
            let mut lower_value = Value::undefined();
            if lower_exists {
                // i. Let lowerValue be ? Get(O, lowerP).
                lower_value = o.get(lower, context)?;
            }
            // f. Let upperExists be ? HasProperty(O, upperP).
            let upper_exists = o.has_property(upper, context)?;
            // g. If upperExists is true, then
            let mut upper_value = Value::undefined();
            if upper_exists {
                // i. Let upperValue be ? Get(O, upperP).
                upper_value = o.get(upper, context)?;
            }
            match (lower_exists, upper_exists) {
                // h. If lowerExists is true and upperExists is true, then
                (true, true) => {
                    // i. Perform ? Set(O, lowerP, upperValue, true).
                    o.set(lower, upper_value, true, context)?;
                    // ii. Perform ? Set(O, upperP, lowerValue, true).
                    o.set(upper, lower_value, true, context)?;
                }
                // i. Else if lowerExists is false and upperExists is true, then
                (false, true) => {
                    // i. Perform ? Set(O, lowerP, upperValue, true).
                    o.set(lower, upper_value, true, context)?;
                    // ii. Perform ? DeletePropertyOrThrow(O, upperP).
                    o.delete_property_or_throw(upper, context)?;
                }
                // j. Else if lowerExists is true and upperExists is false, then
                (true, false) => {
                    // i. Perform ? DeletePropertyOrThrow(O, lowerP).
                    o.delete_property_or_throw(lower, context)?;
                    // ii. Perform ? Set(O, upperP, lowerValue, true).
                    o.set(upper, lower_value, true, context)?;
                }
                // k. Else,
                (false, false) => {
                    // i. Assert: lowerExists and upperExists are both false.
                    // ii. No action is required.
                }
            }

            // l. Set lower to lower + 1.
            lower += 1;
        }
        // 6. Return O.
        Ok(o.into())
    }

    /// `Array.prototype.shift()`
    ///
    /// The first element of the array is removed from the array and returned.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.shift
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/shift
    pub(crate) fn shift(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If len = 0, then
        if len == 0 {
            // a. Perform ? Set(O, "length", +0𝔽, true).
            o.set("length", 0, true, context)?;
            // b. Return undefined.
            return Ok(Value::undefined());
        }
        // 4. Let first be ? Get(O, "0").
        let first = o.get(0, context)?;
        // 5. Let k be 1.
        // 6. Repeat, while k < len,
        for k in 1..len {
            // a. Let from be ! ToString(𝔽(k)).
            let from = k;
            // b. Let to be ! ToString(𝔽(k - 1)).
            let to = k - 1;
            // c. Let fromPresent be ? HasProperty(O, from).
            let from_present = o.has_property(from, context)?;
            // d. If fromPresent is true, then
            if from_present {
                // i. Let fromVal be ? Get(O, from).
                let from_val = o.get(from, context)?;
                // ii. Perform ? Set(O, to, fromVal, true).
                o.set(to, from_val, true, context)?;
            // e. Else,
            } else {
                // i. Assert: fromPresent is false.
                // ii. Perform ? DeletePropertyOrThrow(O, to).
                o.delete_property_or_throw(to, context)?;
            }
            // f. Set k to k + 1.
        }
        // 7. Perform ? DeletePropertyOrThrow(O, ! ToString(𝔽(len - 1))).
        o.delete_property_or_throw(len - 1, context)?;
        // 8. Perform ? Set(O, "length", 𝔽(len - 1), true).
        o.set("length", len - 1, true, context)?;
        // 9. Return first.
        Ok(first)
    }

    /// `Array.prototype.unshift( ...items )`
    ///
    /// The arguments are prepended to the start of the array, such that their order
    /// within the array is the same as the order in which they appear in the
    /// argument list.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.unshift
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift
    pub(crate) fn unshift(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)? as u64;
        // 3. Let argCount be the number of elements in items.
        let arg_count = args.len() as u64;
        // 4. If argCount > 0, then
        if arg_count > 0 {
            // a. If len + argCount > 2^53 - 1, throw a TypeError exception.
            if len + arg_count > 2u64.pow(53) - 1 {
                return context.throw_type_error(
                    "length + number of arguments exceeds the max safe integer limit",
                );
            }
            // b. Let k be len.
            let mut k = len;
            // c. Repeat, while k > 0,
            while k > 0 {
                // i. Let from be ! ToString(𝔽(k - 1)).
                let from = k - 1;
                // ii. Let to be ! ToString(𝔽(k + argCount - 1)).
                let to = k + arg_count - 1;
                // iii. Let fromPresent be ? HasProperty(O, from).
                let from_present = o.has_property(from, context)?;
                // iv. If fromPresent is true, then
                if from_present {
                    // 1. Let fromValue be ? Get(O, from).
                    let from_value = o.get(from, context)?;
                    // 2. Perform ? Set(O, to, fromValue, true).
                    o.set(to, from_value, true, context)?;
                // v. Else,
                } else {
                    // 1. Assert: fromPresent is false.
                    // 2. Perform ? DeletePropertyOrThrow(O, to).
                    o.delete_property_or_throw(to, context)?;
                }
                // vi. Set k to k - 1.
                k -= 1;
            }
            // d. Let j be +0𝔽.
            // e. For each element E of items, do
            for (j, e) in args.iter().enumerate() {
                // i. Perform ? Set(O, ! ToString(j), E, true).
                o.set(j, e, true, context)?;
                // ii. Set j to j + 1𝔽.
            }
        }
        // 5. Perform ? Set(O, "length", 𝔽(len + argCount), true).
        o.set("length", len + arg_count, true, context)?;
        // 6. Return 𝔽(len + argCount).
        Ok((len + arg_count).into())
    }

    /// `Array.prototype.every( callback, [ thisArg ] )`
    ///
    /// The every method executes the provided callback function once for each
    /// element present in the array until it finds the one where callback returns
    /// a falsy value. It returns `false` if it finds such element, otherwise it
    /// returns `true`.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.every
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every
    pub(crate) fn every(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = if let Some(arg) = args
            .get(0)
            .and_then(Value::as_object)
            .filter(GcObject::is_callable)
        {
            arg
        } else {
            return context.throw_type_error("Array.prototype.every: callback is not callable");
        };

        let this_arg = args.get(1).cloned().unwrap_or_default();

        // 4. Let k be 0.
        // 5. Repeat, while k < len,
        for k in 0..len {
            // a. Let Pk be ! ToString(𝔽(k)).
            // b. Let kPresent be ? HasProperty(O, Pk).
            let k_present = o.has_property(k, context)?;
            // c. If kPresent is true, then
            if k_present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(k, context)?;
                // ii. Let testResult be ! ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
                let test_result = callback
                    .call(&this_arg, &[k_value, k.into(), o.clone().into()], context)?
                    .to_boolean();
                // iii. If testResult is false, return false.
                if !test_result {
                    return Ok(Value::from(false));
                }
            }
            // d. Set k to k + 1.
        }
        // 6. Return true.
        Ok(Value::from(true))
    }

    /// `Array.prototype.map( callback, [ thisArg ] )`
    ///
    /// For each element in the array the callback function is called, and a new
    /// array is constructed from the return values of these calls.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.map
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map
    pub(crate) fn map(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = args.get(0).cloned().unwrap_or_default();
        if !callback.is_function() {
            return context.throw_type_error("Array.prototype.map: Callbackfn is not callable");
        }

        // 4. Let A be ? ArraySpeciesCreate(O, len).
        let a = Self::array_species_create(&o, len, context)?;

        let this_arg = args.get(1).cloned().unwrap_or_default();

        // 5. Let k be 0.
        // 6. Repeat, while k < len,
        for k in 0..len {
            // a. Let Pk be ! ToString(𝔽(k)).
            // b. Let k_present be ? HasProperty(O, Pk).
            let k_present = o.has_property(k, context)?;
            // c. If k_present is true, then
            if k_present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(k, context)?;
                // ii. Let mappedValue be ? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »).
                let mapped_value =
                    context.call(&callback, &this_arg, &[k_value, k.into(), this.into()])?;
                // iii. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
                a.create_data_property_or_throw(k, mapped_value, context)?;
            }
            // d. Set k to k + 1.
        }
        // 7. Return A.
        Ok(a.into())
    }

    /// `Array.prototype.indexOf( searchElement[, fromIndex ] )`
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.indexof
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf
    pub(crate) fn index_of(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)? as i64;

        // 3. If len is 0, return -1𝔽.
        if len == 0 {
            return Ok(Value::from(-1));
        }

        // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
        let n = args
            .get(1)
            .cloned()
            .unwrap_or_default()
            .to_integer_or_infinity(context)?;
        // 5. Assert: If fromIndex is undefined, then n is 0.
        let n = match n {
            // 6. If n is +∞, return -1𝔽.
            IntegerOrInfinity::PositiveInfinity => return Ok(Value::from(-1)),
            // 7. Else if n is -∞, set n to 0.
            IntegerOrInfinity::NegativeInfinity => 0,
            IntegerOrInfinity::Integer(value) => value,
        };

        // 8. If n ≥ 0, then
        let mut k;
        if n >= 0 {
            // a. Let k be n.
            k = n
        // 9. Else,
        } else {
            // a. Let k be len + n.
            k = len + n;
            // b. If k < 0, set k to 0.
            if k < 0 {
                k = 0;
            }
        };

        let search_element = args.get(0).cloned().unwrap_or_default();

        // 10. Repeat, while k < len,
        while k < len {
            // a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
            let k_present = o.has_property(k, context)?;
            // b. If kPresent is true, then
            if k_present {
                // i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
                let element_k = o.get(k, context)?;
                // ii. Let same be IsStrictlyEqual(searchElement, elementK).
                // iii. If same is true, return 𝔽(k).
                if search_element.strict_equals(&element_k) {
                    return Ok(Value::from(k));
                }
            }
            // c. Set k to k + 1.
            k += 1;
        }
        // 11. Return -1𝔽.
        Ok(Value::from(-1))
    }

    /// `Array.prototype.lastIndexOf( searchElement[, fromIndex ] )`
    ///
    ///
    /// lastIndexOf compares searchElement to the elements of the array in descending order
    /// using the Strict Equality Comparison algorithm, and if found at one or more indices,
    /// returns the largest such index; otherwise, -1 is returned.
    ///
    /// The optional second argument fromIndex defaults to the array's length minus one
    /// (i.e. the whole array is searched). If it is greater than or equal to the length of the array,
    /// the whole array will be searched. If it is negative, it is used as the offset from the end
    /// of the array to compute fromIndex. If the computed index is less than 0, -1 is returned.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.lastindexof
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf
    pub(crate) fn last_index_of(
        this: &Value,
        args: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)? as i64;

        // 3. If len is 0, return -1𝔽.
        if len == 0 {
            return Ok(Value::from(-1));
        }

        // 4. If fromIndex is present, let n be ? ToIntegerOrInfinity(fromIndex); else let n be len - 1.
        let n = if let Some(from_index) = args.get(1) {
            from_index.to_integer_or_infinity(context)?
        } else {
            IntegerOrInfinity::Integer(len - 1)
        };

        let mut k = match n {
            // 5. If n is -∞, return -1𝔽.
            IntegerOrInfinity::NegativeInfinity => return Ok(Value::from(-1)),
            // 6. If n ≥ 0, then
            //     a. Let k be min(n, len - 1).
            IntegerOrInfinity::Integer(n) if n >= 0 => min(n, len - 1),
            IntegerOrInfinity::PositiveInfinity => len - 1,
            // 7. Else,
            //     a. Let k be len + n.
            IntegerOrInfinity::Integer(n) => len + n,
        };

        let search_element = args.get(0).cloned().unwrap_or_default();

        // 8. Repeat, while k ≥ 0,
        while k >= 0 {
            // a. Let kPresent be ? HasProperty(O, ! ToString(𝔽(k))).
            let k_present = o.has_property(k, context)?;
            // b. If kPresent is true, then
            if k_present {
                // i. Let elementK be ? Get(O, ! ToString(𝔽(k))).
                let element_k = o.get(k, context)?;
                // ii. Let same be IsStrictlyEqual(searchElement, elementK).
                // iii. If same is true, return 𝔽(k).
                if Value::strict_equals(&search_element, &element_k) {
                    return Ok(Value::from(k));
                }
            }
            // c. Set k to k - 1.
            k -= 1;
        }
        // 9. Return -1𝔽.
        Ok(Value::from(-1))
    }

    /// `Array.prototype.find( callback, [thisArg] )`
    ///
    /// The find method executes the callback function once for each index of the array
    /// until the callback returns a truthy value. If so, find immediately returns the value
    /// of that element. Otherwise, find returns undefined.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.find
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find
    pub(crate) fn find(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. If IsCallable(predicate) is false, throw a TypeError exception.
        let predicate = match args.get(0).and_then(Value::as_object) {
            Some(predicate) if predicate.is_callable() => predicate,
            _ => {
                return context.throw_type_error("Array.prototype.find: predicate is not callable")
            }
        };

        let this_arg = args.get(1).cloned().unwrap_or_default();

        // 4. Let k be 0.
        let mut k = 0;
        // 5. Repeat, while k < len,
        while k < len {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Let kValue be ? Get(O, Pk).
            let k_value = o.get(pk, context)?;
            // c. Let testResult be ! ToBoolean(? Call(predicate, thisArg, « kValue, 𝔽(k), O »)).
            let test_result = predicate
                .call(
                    &this_arg,
                    &[k_value.clone(), k.into(), o.clone().into()],
                    context,
                )?
                .to_boolean();
            // d. If testResult is true, return kValue.
            if test_result {
                return Ok(k_value);
            }
            // e. Set k to k + 1.
            k += 1;
        }
        // 6. Return undefined.
        Ok(Value::undefined())
    }

    /// `Array.prototype.findIndex( predicate [ , thisArg ] )`
    ///
    /// This method executes the provided predicate function for each element of the array.
    /// If the predicate function returns `true` for an element, this method returns the index of the element.
    /// If all elements return `false`, the value `-1` is returned.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.findindex
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex
    pub(crate) fn find_index(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. If IsCallable(predicate) is false, throw a TypeError exception.
        let predicate = match args.get(0).and_then(Value::as_object) {
            Some(predicate) if predicate.is_callable() => predicate,
            _ => {
                return context
                    .throw_type_error("Array.prototype.reduce: predicate is not callable")
            }
        };

        let this_arg = args.get(1).cloned().unwrap_or_default();

        // 4. Let k be 0.
        let mut k = 0;
        // 5. Repeat, while k < len,
        while k < len {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Let kValue be ? Get(O, Pk).
            let k_value = o.get(pk, context)?;
            // c. Let testResult be ! ToBoolean(? Call(predicate, thisArg, « kValue, 𝔽(k), O »)).
            let test_result = predicate
                .call(&this_arg, &[k_value, k.into(), o.clone().into()], context)?
                .to_boolean();
            // d. If testResult is true, return 𝔽(k).
            if test_result {
                return Ok(Value::from(k));
            }
            // e. Set k to k + 1.
            k += 1;
        }
        // 6. Return -1𝔽.
        Ok(Value::from(-1))
    }

    /// `Array.prototype.flat( [depth] )`
    ///
    /// This method creates a new array with all sub-array elements concatenated into it
    /// recursively up to the specified depth.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.flat
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flat
    pub(crate) fn flat(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ToObject(this value)
        let o = this.to_object(context)?;

        // 2. Let sourceLen be LengthOfArrayLike(O)
        let source_len = o.length_of_array_like(context)?;

        // 3. Let depthNum be 1
        let mut depth_num = 1;

        // 4. If depth is not undefined, then set depthNum to IntegerOrInfinity(depth)
        if let Some(depth) = args.get(0) {
            // a. Set depthNum to ? ToIntegerOrInfinity(depth).
            // b. If depthNum < 0, set depthNum to 0.
            match depth.to_integer_or_infinity(context)? {
                IntegerOrInfinity::Integer(value) if value >= 0 => depth_num = value as u64,
                IntegerOrInfinity::PositiveInfinity => depth_num = u64::MAX,
                _ => depth_num = 0,
            }
        };

        // 5. Let A be ArraySpeciesCreate(O, 0)
        let a = Self::array_species_create(&o, 0, context)?;

        // 6. Perform ? FlattenIntoArray(A, O, sourceLen, 0, depthNum)
        Self::flatten_into_array(
            &a,
            &o,
            source_len as u64,
            0,
            depth_num,
            None,
            &Value::undefined(),
            context,
        )?;

        Ok(a.into())
    }

    /// `Array.prototype.flatMap( callback, [ thisArg ] )`
    ///
    /// This method returns a new array formed by applying a given callback function to
    /// each element of the array, and then flattening the result by one level. It is
    /// identical to a `map()` followed by a `flat()` of depth 1, but slightly more
    /// efficient than calling those two methods separately.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.flatMap
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flatMap
    pub(crate) fn flat_map(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ToObject(this value)
        let o = this.to_object(context)?;

        // 2. Let sourceLen be LengthOfArrayLike(O)
        let source_len = o.length_of_array_like(context)?;

        // 3. If ! IsCallable(mapperFunction) is false, throw a TypeError exception.
        let mapper_function = args.get(0).cloned().unwrap_or_default();
        if !mapper_function.is_function() {
            return context.throw_type_error("flatMap mapper function is not callable");
        }

        // 4. Let A be ? ArraySpeciesCreate(O, 0).
        let a = Self::array_species_create(&o, 0, context)?;

        // 5. Perform ? FlattenIntoArray(A, O, sourceLen, 0, 1, mapperFunction, thisArg).
        Self::flatten_into_array(
            &a,
            &o,
            source_len as u64,
            0,
            1,
            Some(mapper_function.as_object().unwrap()),
            &args.get(1).cloned().unwrap_or_default(),
            context,
        )?;

        // 6. Return A
        Ok(a.into())
    }

    /// Abstract method `FlattenIntoArray`.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-flattenintoarray
    #[allow(clippy::too_many_arguments)]
    fn flatten_into_array(
        target: &GcObject,
        source: &GcObject,
        source_len: u64,
        start: u64,
        depth: u64,
        mapper_function: Option<GcObject>,
        this_arg: &Value,
        context: &mut Context,
    ) -> Result<u64> {
        // 1. Assert target is Object
        // 2. Assert source is Object

        // 3. Assert if mapper_function is present, then:
        // - IsCallable(mapper_function) is true
        // - thisArg is present
        // - depth is 1

        // 4. Let targetIndex be start
        let mut target_index = start;

        // 5. Let sourceIndex be 0
        let mut source_index = 0;

        // 6. Repeat, while R(sourceIndex) < sourceLen
        while source_index < source_len {
            // a. Let P be ToString(sourceIndex)
            let p = source_index;

            // b. Let exists be ? HasProperty(source, P).
            let exists = source.has_property(p, context)?;
            // c. If exists is true, then
            if exists {
                // i. Let element be Get(source, P)
                let mut element = source.get(p, context)?;

                // ii. If mapperFunction is present, then
                if let Some(ref mapper_function) = mapper_function {
                    // 1. Set element to ? Call(mapperFunction, thisArg, <<element, sourceIndex, source>>)
                    element = mapper_function.call(
                        this_arg,
                        &[element, source_index.into(), source.clone().into()],
                        context,
                    )?;
                }

                // iii. Let shouldFlatten be false
                let mut should_flatten = false;

                // iv. If depth > 0, then
                if depth > 0 {
                    // 1. Set shouldFlatten to ? IsArray(element).
                    should_flatten = element.is_array(context)?;
                }

                // v. If shouldFlatten is true
                if should_flatten {
                    // For `should_flatten` to be true, element must be an object.
                    let element = element.as_object().unwrap();

                    // 1. If depth is +Infinity let newDepth be +Infinity
                    let new_depth = if depth == u64::MAX {
                        u64::MAX
                    // 2. Else, let newDepth be depth - 1
                    } else {
                        depth - 1
                    };

                    // 3. Let elementLen be ? LengthOfArrayLike(element)
                    let element_len = element.length_of_array_like(context)?;

                    // 4. Set targetIndex to ? FlattenIntoArray(target, element, elementLen, targetIndex, newDepth)
                    target_index = Self::flatten_into_array(
                        target,
                        &element,
                        element_len as u64,
                        target_index,
                        new_depth,
                        None,
                        &Value::undefined(),
                        context,
                    )?;

                // vi. Else
                } else {
                    // 1. If targetIndex >= 2^53 - 1, throw a TypeError exception
                    if target_index >= Number::MAX_SAFE_INTEGER as u64 {
                        return Err(context
                            .construct_type_error("Target index exceeded max safe integer value"));
                    }

                    // 2. Perform ? CreateDataPropertyOrThrow(target, targetIndex, element)
                    target.create_data_property_or_throw(target_index, element, context)?;

                    // 3. Set targetIndex to targetIndex + 1
                    target_index += 1;
                }
            }
            // d. Set sourceIndex to sourceIndex + 1
            source_index += 1;
        }

        // 7. Return targetIndex
        Ok(target_index)
    }

    /// `Array.prototype.fill( value[, start[, end]] )`
    ///
    /// The method fills (modifies) all the elements of an array from start index (default 0)
    /// to an end index (default array length) with a static value. It returns the modified array.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.fill
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill
    pub(crate) fn fill(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        // 4. If relativeStart is -∞, let k be 0.
        // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
        // 6. Else, let k be min(relativeStart, len).
        let mut k = Self::get_relative_start(context, args.get(1), len)?;

        // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be ? ToIntegerOrInfinity(end).
        // 8. If relativeEnd is -∞, let final be 0.
        // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        // 10. Else, let final be min(relativeEnd, len).
        let final_ = Self::get_relative_end(context, args.get(2), len)?;

        let value = args.get(0).cloned().unwrap_or_default();

        // 11. Repeat, while k < final,
        while k < final_ {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Perform ? Set(O, Pk, value, true).
            o.set(pk, value.clone(), true, context)?;
            // c. Set k to k + 1.
            k += 1;
        }
        // 12. Return O.
        Ok(o.into())
    }

    /// `Array.prototype.includes( valueToFind [, fromIndex] )`
    ///
    /// Determines whether an array includes a certain value among its entries, returning `true` or `false` as appropriate.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.includes
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes
    pub(crate) fn includes_value(
        this: &Value,
        args: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)? as i64;

        // 3. If len is 0, return false.
        if len == 0 {
            return Ok(Value::from(false));
        }

        // 4. Let n be ? ToIntegerOrInfinity(fromIndex).
        let n = args
            .get(1)
            .cloned()
            .unwrap_or_default()
            .to_integer_or_infinity(context)?;
        // 5. Assert: If fromIndex is undefined, then n is 0.
        // 6. If n is +∞, return false.
        // 7. Else if n is -∞, set n to 0.
        let n = match n {
            IntegerOrInfinity::PositiveInfinity => return Ok(Value::from(false)),
            IntegerOrInfinity::NegativeInfinity => 0,
            IntegerOrInfinity::Integer(value) => value,
        };

        // 8. If n ≥ 0, then
        let mut k;
        if n >= 0 {
            // a. Let k be n.
            k = n
        // 9. Else,
        } else {
            // a. Let k be len + n.
            k = len + n;
            // b. If k < 0, set k to 0.
            if k < 0 {
                k = 0;
            }
        }

        let search_element = args.get(0).cloned().unwrap_or_default();

        // 10. Repeat, while k < len,
        while k < len {
            // a. Let elementK be ? Get(O, ! ToString(𝔽(k))).
            let element_k = o.get(k, context)?;
            // b. If SameValueZero(searchElement, elementK) is true, return true.
            if Value::same_value_zero(&search_element, &element_k) {
                return Ok(Value::from(true));
            }
            // c. Set k to k + 1.
            k += 1;
        }
        // 11. Return false.
        Ok(Value::from(false))
    }

    /// `Array.prototype.slice( [begin[, end]] )`
    ///
    /// The slice method takes two arguments, start and end, and returns an array containing the
    /// elements of the array from element start up to, but not including, element end (or through the
    /// end of the array if end is undefined). If start is negative, it is treated as length + start
    /// where length is the length of the array. If end is negative, it is treated as length + end where
    /// length is the length of the array.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.slice
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice
    pub(crate) fn slice(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. Let relativeStart be ? ToIntegerOrInfinity(start).
        // 4. If relativeStart is -∞, let k be 0.
        // 5. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
        // 6. Else, let k be min(relativeStart, len).
        let mut k = Self::get_relative_start(context, args.get(0), len)?;

        // 7. If end is undefined, let relativeEnd be len; else let relativeEnd be ? ToIntegerOrInfinity(end).
        // 8. If relativeEnd is -∞, let final be 0.
        // 9. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        // 10. Else, let final be min(relativeEnd, len).
        let final_ = Self::get_relative_end(context, args.get(1), len)?;

        // 11. Let count be max(final - k, 0).
        let count = final_.saturating_sub(k);

        // 12. Let A be ? ArraySpeciesCreate(O, count).
        let a = Self::array_species_create(&o, count, context)?;

        // 13. Let n be 0.
        let mut n: u64 = 0;
        // 14. Repeat, while k < final,
        while k < final_ {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Let kPresent be ? HasProperty(O, Pk).
            let k_present = o.has_property(pk, context)?;
            // c. If kPresent is true, then
            if k_present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(pk, context)?;
                // ii. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(n)), kValue).
                a.create_data_property_or_throw(n, k_value, context)?;
            }
            // d. Set k to k + 1.
            k += 1;
            // e. Set n to n + 1.
            n += 1;
        }

        // 15. Perform ? Set(A, "length", 𝔽(n), true).
        a.set("length", n, true, context)?;

        // 16. Return A.
        Ok(a.into())
    }

    /// `Array.prototype.filter( callback, [ thisArg ] )`
    ///
    /// For each element in the array the callback function is called, and a new
    /// array is constructed for every value whose callback returned a truthy value.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.filter
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter
    pub(crate) fn filter(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let length = o.length_of_array_like(context)?;

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = args
            .get(0)
            .map(|a| a.to_object(context))
            .transpose()?
            .ok_or_else(|| {
                context.construct_type_error(
                    "missing argument 0 when calling function Array.prototype.filter",
                )
            })?;
        let this_val = args.get(1).cloned().unwrap_or_else(Value::undefined);

        if !callback.is_callable() {
            return context.throw_type_error("the callback must be callable");
        }

        // 4. Let A be ? ArraySpeciesCreate(O, 0).
        let a = Self::array_species_create(&o, 0, context)?;

        // 5. Let k be 0.
        // 6. Let to be 0.
        let mut to = 0u32;
        // 7. Repeat, while k < len,
        for idx in 0..length {
            // a. Let Pk be ! ToString(𝔽(k)).
            // b. Let kPresent be ? HasProperty(O, Pk).
            // c. If kPresent is true, then
            if o.has_property(idx, context)? {
                // i. Let kValue be ? Get(O, Pk).
                let element = o.get(idx, context)?;

                let args = [element.clone(), Value::from(idx), Value::from(o.clone())];

                // ii. Let selected be ! ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
                let selected = callback.call(&this_val, &args, context)?.to_boolean();

                // iii. If selected is true, then
                if selected {
                    // 1. Perform ? CreateDataPropertyOrThrow(A, ! ToString(𝔽(to)), kValue).
                    a.create_data_property_or_throw(to, element, context)?;
                    // 2. Set to to to + 1.
                    to += 1;
                }
            }
        }

        // 8. Return A.
        Ok(a.into())
    }

    /// Array.prototype.some ( callbackfn [ , thisArg ] )
    ///
    /// The some method tests whether at least one element in the array passes
    /// the test implemented by the provided callback function. It returns a Boolean value,
    /// true if the callback function returns a truthy value for at least one element
    /// in the array. Otherwise, false.
    ///
    /// Caution: Calling this method on an empty array returns false for any condition!
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.some
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some
    pub(crate) fn some(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;
        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;
        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = if let Some(arg) = args
            .get(0)
            .and_then(Value::as_object)
            .filter(GcObject::is_callable)
        {
            arg
        } else {
            return context.throw_type_error("Array.prototype.some: callback is not callable");
        };

        // 4. Let k be 0.
        // 5. Repeat, while k < len,
        for k in 0..len {
            // a. Let Pk be ! ToString(𝔽(k)).
            // b. Let kPresent be ? HasProperty(O, Pk).
            let k_present = o.has_property(k, context)?;
            // c. If kPresent is true, then
            if k_present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(k, context)?;
                // ii. Let testResult be ! ToBoolean(? Call(callbackfn, thisArg, « kValue, 𝔽(k), O »)).
                let this_arg = args.get(1).cloned().unwrap_or_default();
                let test_result = callback
                    .call(&this_arg, &[k_value, k.into(), o.clone().into()], context)?
                    .to_boolean();
                // iii. If testResult is true, return true.
                if test_result {
                    return Ok(Value::from(true));
                }
            }
            // d. Set k to k + 1.
        }
        // 6. Return false.
        Ok(Value::from(false))
    }

    /// `Array.prototype.reduce( callbackFn [ , initialValue ] )`
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.reduce
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduce
    pub(crate) fn reduce(this: &Value, args: &[Value], context: &mut Context) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = match args.get(0).and_then(Value::as_object) {
            Some(callback) if callback.is_callable() => callback,
            _ => {
                return context
                    .throw_type_error("Array.prototype.reduce: callback function is not callable")
            }
        };

        // 4. If len = 0 and initialValue is not present, throw a TypeError exception.
        if len == 0 && args.get(1).is_none() {
            return context.throw_type_error(
                "Array.prototype.reduce: called on an empty array and with no initial value",
            );
        }

        // 5. Let k be 0.
        let mut k = 0;
        // 6. Let accumulator be undefined.
        let mut accumulator = Value::undefined();

        // 7. If initialValue is present, then
        if let Some(initial_value) = args.get(1) {
            // a. Set accumulator to initialValue.
            accumulator = initial_value.clone();
        // 8. Else,
        } else {
            // a. Let kPresent be false.
            let mut k_present = false;
            // b. Repeat, while kPresent is false and k < len,
            while !k_present && k < len {
                // i. Let Pk be ! ToString(𝔽(k)).
                let pk = k;
                // ii. Set kPresent to ? HasProperty(O, Pk).
                k_present = o.has_property(pk, context)?;
                // iii. If kPresent is true, then
                if k_present {
                    // 1. Set accumulator to ? Get(O, Pk).
                    accumulator = o.get(pk, context)?;
                }
                // iv. Set k to k + 1.
                k += 1;
            }
            // c. If kPresent is false, throw a TypeError exception.
            if !k_present {
                return context.throw_type_error(
                    "Array.prototype.reduce: called on an empty array and with no initial value",
                );
            }
        }

        // 9. Repeat, while k < len,
        while k < len {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Let kPresent be ? HasProperty(O, Pk).
            let k_present = o.has_property(pk, context)?;
            // c. If kPresent is true, then
            if k_present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(pk, context)?;
                // ii. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
                accumulator = callback.call(
                    &Value::undefined(),
                    &[accumulator, k_value, k.into(), o.clone().into()],
                    context,
                )?;
            }
            // d. Set k to k + 1.
            k += 1;
        }

        // 10. Return accumulator.
        Ok(accumulator)
    }

    /// `Array.prototype.reduceRight( callbackFn [ , initialValue ] )`
    ///
    /// The reduceRight method traverses right to left starting from the last defined value in the array,
    /// accumulating a value using a given callback function. It returns the accumulated value.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.reduceright
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight
    pub(crate) fn reduce_right(
        this: &Value,
        args: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. If IsCallable(callbackfn) is false, throw a TypeError exception.
        let callback = match args.get(0).and_then(Value::as_object) {
            Some(callback) if callback.is_callable() => callback,
            _ => {
                return context.throw_type_error(
                    "Array.prototype.reduceRight: callback function is not callable",
                )
            }
        };

        // 4. If len is 0 and initialValue is not present, throw a TypeError exception.
        if len == 0 && args.get(1).is_none() {
            return context.throw_type_error(
                "Array.prototype.reduceRight: called on an empty array and with no initial value",
            );
        }

        // 5. Let k be len - 1.
        let mut k = len as i64 - 1;
        // 6. Let accumulator be undefined.
        let mut accumulator = Value::undefined();
        // 7. If initialValue is present, then
        if let Some(initial_value) = args.get(1) {
            // a. Set accumulator to initialValue.
            accumulator = initial_value.clone();
        // 8. Else,
        } else {
            // a. Let kPresent be false.
            let mut k_present = false;
            // b. Repeat, while kPresent is false and k ≥ 0,
            while !k_present && k >= 0 {
                // i. Let Pk be ! ToString(𝔽(k)).
                let pk = k;
                // ii. Set kPresent to ? HasProperty(O, Pk).
                k_present = o.has_property(pk, context)?;
                // iii. If kPresent is true, then
                if k_present {
                    // 1. Set accumulator to ? Get(O, Pk).
                    accumulator = o.get(pk, context)?;
                }
                // iv. Set k to k - 1.
                k -= 1;
            }
            // c. If kPresent is false, throw a TypeError exception.
            if !k_present {
                return context.throw_type_error(
                    "Array.prototype.reduceRight: called on an empty array and with no initial value",
                );
            }
        }

        // 9. Repeat, while k ≥ 0,
        while k >= 0 {
            // a. Let Pk be ! ToString(𝔽(k)).
            let pk = k;
            // b. Let kPresent be ? HasProperty(O, Pk).
            let k_present = o.has_property(pk, context)?;
            // c. If kPresent is true, then
            if k_present {
                // i. Let kValue be ? Get(O, Pk).
                let k_value = o.get(pk, context)?;
                // ii. Set accumulator to ? Call(callbackfn, undefined, « accumulator, kValue, 𝔽(k), O »).
                accumulator = callback.call(
                    &Value::undefined(),
                    &[accumulator.clone(), k_value, k.into(), o.clone().into()],
                    context,
                )?;
            }
            // d. Set k to k - 1.
            k -= 1;
        }

        // 10. Return accumulator.
        Ok(accumulator)
    }

    /// `Array.prototype.copyWithin ( target, start [ , end ] )`
    ///
    /// The copyWithin() method shallow copies part of an array to another location
    /// in the same array and returns it without modifying its length.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.copywithin
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin
    pub(crate) fn copy_within(
        this: &Value,
        args: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        // 1. Let O be ? ToObject(this value).
        let o = this.to_object(context)?;

        // 2. Let len be ? LengthOfArrayLike(O).
        let len = o.length_of_array_like(context)?;

        // 3. Let relativeTarget be ? ToIntegerOrInfinity(target).
        // 4. If relativeTarget is -∞, let to be 0.
        // 5. Else if relativeTarget < 0, let to be max(len + relativeTarget, 0).
        // 6. Else, let to be min(relativeTarget, len).
        let mut to = Self::get_relative_start(context, args.get(0), len)? as i64;

        // 7. Let relativeStart be ? ToIntegerOrInfinity(start).
        // 8. If relativeStart is -∞, let from be 0.
        // 9. Else if relativeStart < 0, let from be max(len + relativeStart, 0).
        // 10. Else, let from be min(relativeStart, len).
        let mut from = Self::get_relative_start(context, args.get(1), len)? as i64;

        // 11. If end is undefined, let relativeEnd be len; else let relativeEnd be ? ToIntegerOrInfinity(end).
        // 12. If relativeEnd is -∞, let final be 0.
        // 13. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
        // 14. Else, let final be min(relativeEnd, len).
        let final_ = Self::get_relative_end(context, args.get(2), len)? as i64;

        // 15. Let count be min(final - from, len - to).
        let mut count = min(final_ - from, len as i64 - to);

        // 16. If from < to and to < from + count, then
        let direction = if from < to && to < from + count {
            // b. Set from to from + count - 1.
            from = from + count - 1;
            // c. Set to to to + count - 1.
            to = to + count - 1;

            // a. Let direction be -1.
            -1
        // 17. Else,
        } else {
            // a. Let direction be 1.
            1
        };

        // 18. Repeat, while count > 0,
        while count > 0 {
            // a. Let fromKey be ! ToString(𝔽(from)).
            let from_key = from;

            // b. Let toKey be ! ToString(𝔽(to)).
            let to_key = to;

            // c. Let fromPresent be ? HasProperty(O, fromKey).
            let from_present = o.has_property(from_key, context)?;
            // d. If fromPresent is true, then
            if from_present {
                // i. Let fromVal be ? Get(O, fromKey).
                let from_val = o.get(from_key, context)?;
                // ii. Perform ? Set(O, toKey, fromVal, true).
                o.set(to_key, from_val, true, context)?;
            // e. Else,
            } else {
                // i. Assert: fromPresent is false.
                // ii. Perform ? DeletePropertyOrThrow(O, toKey).
                o.delete_property_or_throw(to_key, context)?;
            }
            // f. Set from to from + direction.
            from += direction;
            // g. Set to to to + direction.
            to += direction;
            // h. Set count to count - 1.
            count -= 1;
        }
        // 19. Return O.
        Ok(o.into())
    }

    /// `Array.prototype.values( )`
    ///
    /// The values method returns an iterable that iterates over the values in the array.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.values
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values
    pub(crate) fn values(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        Ok(ArrayIterator::create_array_iterator(
            context,
            this.clone(),
            ArrayIterationKind::Value,
        ))
    }

    /// `Array.prototype.keys( )`
    ///
    /// The keys method returns an iterable that iterates over the indexes in the array.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.values
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values
    pub(crate) fn keys(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        Ok(ArrayIterator::create_array_iterator(
            context,
            this.clone(),
            ArrayIterationKind::Key,
        ))
    }

    /// `Array.prototype.entries( )`
    ///
    /// The entries method returns an iterable that iterates over the key-value pairs in the array.
    ///
    /// More information:
    ///  - [ECMAScript reference][spec]
    ///  - [MDN documentation][mdn]
    ///
    /// [spec]: https://tc39.es/ecma262/#sec-array.prototype.values
    /// [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values
    pub(crate) fn entries(this: &Value, _: &[Value], context: &mut Context) -> Result<Value> {
        Ok(ArrayIterator::create_array_iterator(
            context,
            this.clone(),
            ArrayIterationKind::KeyAndValue,
        ))
    }

    /// Represents the algorithm to calculate `relativeStart` (or `k`) in array functions.
    pub(super) fn get_relative_start(
        context: &mut Context,
        arg: Option<&Value>,
        len: usize,
    ) -> Result<usize> {
        // 1. Let relativeStart be ? ToIntegerOrInfinity(start).
        let relative_start = arg
            .cloned()
            .unwrap_or_default()
            .to_integer_or_infinity(context)?;
        match relative_start {
            // 2. If relativeStart is -∞, let k be 0.
            IntegerOrInfinity::NegativeInfinity => Ok(0),
            // 3. Else if relativeStart < 0, let k be max(len + relativeStart, 0).
            IntegerOrInfinity::Integer(i) if i < 0 => Ok(max(len as i64 + i, 0) as usize),
            // Both `as` casts are safe as both variables are non-negative
            // 4. Else, let k be min(relativeStart, len).
            IntegerOrInfinity::Integer(i) => Ok(min(i, len as i64) as usize),

            // Special case - postive infinity. `len` is always smaller than +inf, thus from (4)
            IntegerOrInfinity::PositiveInfinity => Ok(len),
        }
    }

    /// Represents the algorithm to calculate `relativeEnd` (or `final`) in array functions.
    pub(super) fn get_relative_end(
        context: &mut Context,
        arg: Option<&Value>,
        len: usize,
    ) -> Result<usize> {
        let default_value = Value::undefined();
        let value = arg.unwrap_or(&default_value);
        // 1. If end is undefined, let relativeEnd be len [and return it]
        if value.is_undefined() {
            Ok(len)
        } else {
            // 1. cont, else let relativeEnd be ? ToIntegerOrInfinity(end).
            let relative_end = value.to_integer_or_infinity(context)?;
            match relative_end {
                // 2. If relativeEnd is -∞, let final be 0.
                IntegerOrInfinity::NegativeInfinity => Ok(0),
                // 3. Else if relativeEnd < 0, let final be max(len + relativeEnd, 0).
                IntegerOrInfinity::Integer(i) if i < 0 => Ok(max(len as i64 + i, 0) as usize),
                // 4. Else, let final be min(relativeEnd, len).
                // Both `as` casts are safe as both variables are non-negative
                IntegerOrInfinity::Integer(i) => Ok(min(i, len as i64) as usize),

                // Special case - postive infinity. `len` is always smaller than +inf, thus from (4)
                IntegerOrInfinity::PositiveInfinity => Ok(len),
            }
        }
    }
}
