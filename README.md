# session-pi
A session-typed pi-calculus interpreter

## Setup

1. Install nix:
```
$ run curl -L https://nixos.org/nix/install | sh
```

2. Initialize the nix environment
```
$ nix-shell
```

3. To build/run
```
$ dune exec --display quiet sessionPi
```
for now, this should just print "Hello, World!" 
