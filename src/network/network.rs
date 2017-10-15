use ::network::ids::*;
use ::builders::ids::RuleId;

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum AlphaDest {
    Alpha(AlphaId),
    Beta(BetaId),
    BetaInter(BetaInter),
    Rule(RuleId)
}

impl Into<DestinationNode> for AlphaId {
    fn into(self) -> DestinationNode {
        DestinationNode::Alpha(self)
    }
}

impl Into<DestinationNode> for BetaId {
    fn into(self) -> DestinationNode {
        DestinationNode::Beta(self)
    }
}

impl Into<DestinationNode> for RuleId {
    fn into(self) -> DestinationNode {
        DestinationNode::Rule(self)
    }

}