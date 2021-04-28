(cd #P"~/works/synth/schmidt/sources/librarian/")

(pushnew #P"~/works/synth/CoreMIDI/"                  asdf:*central-registry* :test (function equalp))
(pushnew #P"~/works/synth/midi/"                      asdf:*central-registry* :test (function equalp))
(pushnew #P"~/works/synth/schmidt/sources/librarian/" asdf:*central-registry* :test (function equalp))

(ql:quickload :com.informatimago.synthesizer.schmidt.librarian)

(in-package :com.informatimago.synthesizer.schmidt)
(in-package :com.informatimago.synthesizer.schmidt.librarian)
