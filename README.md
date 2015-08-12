Oboe-Lang
=========

Oboe is small language for designing synthesizers. The current
back-end targets Csound, in the future SuperCollider may be 
supported.

Oboe aims to overcome the poor support for modularity in the 
current generation of synthesizer design languages (Csound 
orchestras, SuperCollider synthdefs) and offer properly reuseable,
nested components not just a single strata of components (ugens).

Oboe is a revision of my previous synthesizer design language 
Ochre. Ochre used traits to compose synthesizer components. 
Composition in Oboe uses *forms* (extensible records - c.f the 
languages Piccola and GLoo) which seems a better fit as Oboe is
roughly a functional language. 




