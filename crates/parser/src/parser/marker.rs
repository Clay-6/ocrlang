use syntax::SyntaxKind;

use crate::event::Event;

use super::Parser;

pub(crate) struct Marker {
    pos: usize,
    completed: bool,
}

pub(crate) struct CompletedMarker {
    pos: usize,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Self {
            pos,
            completed: false,
        }
    }

    pub(crate) fn complete(mut self, p: &mut Parser, kind: SyntaxKind) -> CompletedMarker {
        self.completed = true;

        let event_at_pos = &mut p.events[self.pos];
        assert_eq!(*event_at_pos, Event::Placeholder);

        *event_at_pos = Event::StartNode {
            kind,
            forward_parent: None,
        };

        p.events.push(Event::FinishNode);

        CompletedMarker { pos: self.pos }
    }
}

impl CompletedMarker {
    pub(crate) fn precede(self, p: &mut Parser) -> Marker {
        let new_m = p.start();

        if let Event::StartNode {
            ref mut forward_parent,
            ..
        } = p.events[self.pos]
        {
            *forward_parent = Some(new_m.pos - self.pos);
        } else {
            unreachable!();
        }

        new_m
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed {
            panic!("Marker must be completed")
        }
    }
}
