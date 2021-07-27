//! This module implements the global `EvalError` object.
//!
//! Indicates an error regarding the global `eval()` function.
//! This exception is not thrown by JavaScript anymore, however
//! the `EvalError` object remains for compatibility.
//!
//! More information:
//!  - [MDN documentation][mdn]
//!  - [ECMAScript reference][spec]
//!
//! [spec]: https://tc39.es/ecma262/#sec-native-error-types-used-in-this-standard-evalerror
//! [mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/EvalError

use crate::{
    builtins::BuiltIn,
    object::{ConstructorBuilder, ObjectData},
    profiler::BoaProfiler,
    property::Attribute,
    string::Constants,
    Context, Result, Value,
};

/// JavaScript `EvalError` impleentation.
#[derive(Debug, Clone, Copy)]
pub(crate) struct EvalError;

impl BuiltIn for EvalError {
    const NAME: &'static str = "EvalError";

    fn attribute() -> Attribute {
        Attribute::WRITABLE | Attribute::NON_ENUMERABLE | Attribute::CONFIGURABLE
    }

    fn init(context: &mut Context) -> (&'static str, Value, Attribute) {
        let _timer = BoaProfiler::global().start_event(Self::NAME, "init");

        let error_prototype = context.standard_objects().error_object().prototype();
        let attribute = Attribute::WRITABLE | Attribute::NON_ENUMERABLE | Attribute::CONFIGURABLE;
        let eval_error_object = ConstructorBuilder::with_standard_object(
            context,
            Self::constructor,
            context.standard_objects().eval_error_object().clone(),
        )
        .name(Self::NAME)
        .length(Self::LENGTH)
        .inherit(error_prototype.into())
        .property(Constants::name(), Self::NAME, attribute)
        .property(Constants::message(), "", attribute)
        .build();

        (Self::NAME, eval_error_object.into(), Self::attribute())
    }
}

impl EvalError {
    /// The amount of arguments this function object takes.
    pub(crate) const LENGTH: usize = 1;

    /// Create a new error object.
    pub(crate) fn constructor(
        new_target: &Value,
        args: &[Value],
        context: &mut Context,
    ) -> Result<Value> {
        let prototype = new_target
            .as_object()
            .and_then(|obj| {
                obj.__get__(&Constants::prototype().into(), obj.clone().into(), context)
                    .map(|o| o.as_object())
                    .transpose()
            })
            .transpose()?
            .unwrap_or_else(|| context.standard_objects().error_object().prototype());
        let obj = context.construct_object();
        obj.set_prototype_instance(prototype.into());
        let this = Value::from(obj);
        if let Some(message) = args.get(0) {
            if !message.is_undefined() {
                this.set_field(
                    Constants::message(),
                    message.to_string(context)?,
                    false,
                    context,
                )?;
            }
        }

        // This value is used by console.log and other routines to match Object type
        // to its Javascript Identifier (global constructor method name)
        this.set_data(ObjectData::Error);
        Ok(this)
    }
}
