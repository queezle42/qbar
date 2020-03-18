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
./bin/run-sway default

# Run the binary directly (mostly used to control the bar via rpc)
./bin/run --help

# Install the binary to ~/.local/bin (this can also install tab completions)
./bin/install
```

## Configuration

All configuration is currently done with command line arguments. The executable uses a command-style interface (e.g. `qbar theme rainbow`) which supports `--help` at every level of the tree. It also provides bash, zsh and fish tab completions.

### Sway

Use the following `status_command`:

```
qbar server swaybar default
```

You can specify a custom set of blocks:

```
qbar server swaybar date cpu network script ~/bin/my_script
```

### i3

i3 runs the status command for every screen that shows a bar. To reuse the output of your primary display you can use mirror mode:

```
# Configure for primary display
qbar server i3bar default

# Configure for other displays
qbar mirror i3bar
```

Theming is not supported on mirrored servers.
