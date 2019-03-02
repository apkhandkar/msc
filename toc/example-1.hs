import FSM
import EClosure

-- a pre-built FSM to test FSM and EClosure on
trans = [ 
    Transition {from=State "A", input=Epsilon,    to=State "B"},
    Transition {from=State "A", input=Epsilon,    to=State "D"},
    Transition {from=State "B", input=Symbol "a", to=State "C"},
    Transition {from=State "C", input=Epsilon,    to=State "F"},
    Transition {from=State "D", input=Symbol "b", to=State "E"},
    Transition {from=State "E", input=Epsilon,    to=State "F"},
    Transition {from=State "F", input=Epsilon,    to=State "G"},
    Transition {from=State "G", input=Epsilon,    to=State "H"},
    Transition {from=State "H", input=Symbol "a", to=State "I"},
    Transition {from=State "H", input=Epsilon,    to=State "K"},
    Transition {from=State "I", input=Epsilon,    to=State "J"},
    Transition {from=State "I", input=Symbol "b", to=State "H"},
    Transition {from=State "K", input=Symbol "b", to=State "L"},
    Transition {from=State "J", input=Epsilon,    to=State "M"},
    Transition {from=State "L", input=Epsilon,    to=State "M"},
    Transition {from=State "M", input=Epsilon,    to=State "G"},
    Transition {from=State "M", input=Epsilon,    to=State "N"},
    Transition {from=State "O", input=Symbol "a", to=State "O"},
    Transition {from=State "P", input=Symbol "b", to=State "P"},
    Transition {from=State "N", input=Epsilon,    to=State "O"},
    Transition {from=State "N", input=Epsilon,    to=State "P"} ]

machine = buildFSMachine trans (State "A") [(State "O"),(State "P")]
