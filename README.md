# qbar

qbar is a status command for [sway](https://swaywm.org/) and [i3](https://i3wm.org/).

## Installation

### Compiling from Source

Building qbar requires [stack](https://haskellstack.org/). To build it run:
```
stack build
```

You can also use the scripts in the `bin`-directory (all scripts will rebuild the project if required):
```
# Launch as swaybar status command while redirecting stderr to shell
./bin/run-sway

# Run the binary directly (mostly used to control the bar via rpc)
./bin/run --help

# Install the binary to ~/.local/bin
./bin/install
```

## Configuration

Custom configuration is currently only possible from Haskell:
```
myConfig :: BarIO ()
myConfig = do
  addBlock dateBlock

main :: IO ()
main = parseOptions >>= runQBar myConfig
```