use std::sync::Arc;

use crate::SrcLoc;

#[derive(Debug, Clone)]
pub struct Name {
    text: Arc<str>,
    srcloc: Option<SrcLoc>,
}

impl Name {
    pub fn new(text: impl Into<Arc<str>>, srcloc: Option<SrcLoc>) -> Self {
        Self {
            text: text.into(),
            srcloc,
        }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn srcloc(&self) -> Option<&SrcLoc> {
        self.srcloc.as_ref()
    }
}
