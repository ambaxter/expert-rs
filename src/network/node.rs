use ::network::ids::*;
use ::builders::ids::RuleId;
use base::DestinationNode;

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum AlphaDest {
    Alpha(AlphaId),
    Beta(BetaId),
    Inter(InterId),
    Rule(RuleId)
}

into_dest!(AlphaDest; AlphaId => Alpha, BetaId => Beta, InterId => Inter, RuleId => Rule);

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum BetaDest {
    Beta(BetaId),
    Inter(InterId),
    Rule(RuleId)
}

into_dest!(BetaDest; BetaId => Beta, InterId => Inter, RuleId => Rule);

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum InterDest {
    Inter(InterId),
    Rule(RuleId)
}

into_dest!(InterDest; InterId => Inter, RuleId => Rule);



