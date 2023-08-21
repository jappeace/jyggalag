[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

> Jyggalag! He is the Prince of Order. Or biscuits...no, order, Order!
> And not in a good way. Bleak. Colorless. Dead. Boring, boring, BORING!

Mostly automated large scale maintenance.

This project makes some basic assumptions on maintenance allowing
you to automate a bunch of things.

It allows setting a template project from which to copy github
actions from.
Planned features:
+ Automatically updating all nix flakes.
  Doing this in one go, has the advantage cache get's more shared across projects,
  saving disk space.


## Usage

update `jyggalag.toml` to suit your needs.


```
cabal run exe -- copy
```

### Tools
Enter the nix shell.
```
nix develop
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
