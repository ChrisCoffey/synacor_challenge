# synacor_challenge
The Synacor Challenge VM

After making it through the [Advent of Code](http://adventofcode.com/)  it was time to take on another problem in Haskell. The Synacor challenge is essentially to build an interpreter for a small assembly language then beat the game w/in the provided binary.

## Running this code
 1. Install the [Haskell Stack] (http://docs.haskellstack.org/en/stable/README.html)
 1. Clone this repo
 1. Start the interpreter
    ```bash
    $> stack build
    $> stack exec Synacor-exe
    ```
 1. Connect the debugger from a different terminal window
    ```bash
    $> telnet localhost 8888
    ```

## Debugger Commands
All debugger commands are submitted via the telnet session, and the resulting output is shown in the interpreter

#### Go
Resumes normal interpreter execution.
```bash
$> go
```

#### Pause
Stops the interpreter from executing & prints current registers
```bash
$> pause
```

#### Step
Slows down and logs the full interpreter execution (instruction, registers & stack)
```bash
$> step
```

#### Break
Transitions interpreter to a paused state as soon as the provided instruction is hit
```bash
$> break <addr>
```

#### Set
Writes the provided value directly to the interpreter's memory, overwriting whichever value was present
```bash 
$>set <addr> <value>
```

Enjoy!

