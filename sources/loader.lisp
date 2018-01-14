(cd #P"~/works/synth/schmidt/sources/")

(push #P"~/works/synth/CoreMIDI/"        asdf:*central-registry*)
(push #P"~/works/synth/schmidt/sources/" asdf:*central-registry*)

(ql:quickload :com.informatimago.synthesizer.schmidt.librarian)
(in-package :com.informatimago.synthesizer.schmidt)
(in-package :com.informatimago.synthesizer.schmidt.librarian)
