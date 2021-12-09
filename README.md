![](logo/brick-final-clearbg-with-text.svg)

`brick` is a Haskell terminal user interface (TUI) programming toolkit.
To use it, you write a pure function that describes how your user
interface should be drawn based on your current application state and
you provide a state transformation function to handle events.

`brick` exposes a declarative API. Unlike most GUI toolkits which
require you to write a long and tedious sequence of "create a widget,
now bind an event handler", `brick` just requires you to describe your
interface using a set of declarative layout combinators.

Under the hood, this library builds upon
[vty](http://hackage.haskell.org/package/vty), so some knowledge of Vty
will be helpful in using this library.

Example
-------

Here's an example interface (see `programs/ReadmeDemo.hs`):

```
withBorderStyle unicode $
borderWithLabel (str "Hello!") $
(center (str "Left") <+> vBorder <+> center (str "Right"))
```

Result:

```
┌─────────Hello!─────────┐
│           │            │
│           │            │
│   Left    │   Right    │
│           │            │
│           │            │
└────────────────────────┘
```

Featured Projects
-----------------

To get an idea of what some people have done with `brick`, check out
these projects. If you have made something and would like me to include
it, get in touch!

| Project | Description |
| ------- | ----------- |
| [`tetris`](https://github.com/SamTay/tetris) | An implementation of the Tetris game |
| [`gotta-go-fast`](https://github.com/callum-oakley/gotta-go-fast) | A typing tutor |
| [`haskell-player`](https://github.com/potomak/haskell-player) | An `afplay` frontend |
| [`mushu`](https://github.com/elaye/mushu) | An `MPD` client |
| [`matterhorn`](https://github.com/matterhorn-chat/matterhorn) | A client for [Mattermost](https://about.mattermost.com/) |
| [`viewprof`](https://github.com/maoe/viewprof) | A GHC profile viewer |
| [`tart`](https://github.com/jtdaugherty/tart) | A mouse-driven ASCII art drawing program |
| [`silly-joy`](https://github.com/rootmos/silly-joy) | An interpreter for Joy |
| [`herms`](https://github.com/jackkiefer/herms) | A command-line tool for managing kitchen recipes |
| [`purebred`](https://github.com/purebred-mua/purebred) | A mail user agent |
| [`2048Haskell`](https://github.com/8Gitbrix/2048Haskell) | An implementation of the 2048 game |
| [`bhoogle`](https://github.com/andrevdm/bhoogle) | A [Hoogle](https://www.haskell.org/hoogle/) client |
| [`clifm`](https://github.com/pasqu4le/clifm) | A file manager |
| [`towerHanoi`](https://github.com/shajenM/projects/tree/master/towerHanoi) | Animated solutions to The Tower of Hanoi |
| [`VOIDSPACE`](https://github.com/ChrisPenner/void-space) | A space-themed typing-tutor game |
| [`solitaire`](https://github.com/ambuc/solitaire) | The card game |
| [`sudoku-tui`](https://github.com/evanrelf/sudoku-tui) | A Sudoku implementation |
| [`summoner-tui`](https://github.com/kowainik/summoner/tree/master/summoner-tui) | An interactive frontend to the Summoner tool |
| [`wrapping-editor`](https://github.com/ta0kira/wrapping-editor) | An embeddable editor with support for Brick |
| [`git-brunch`](https://github.com/andys8/git-brunch) | A git branch checkout utility |
| [`hascard`](https://github.com/Yvee1/hascard) | A program for reviewing "flash card" notes |
| [`ttyme`](https://github.com/evuez/ttyme) | A TUI for [Harvest](https://www.getharvest.com/) |
| [`ghcup`](https://www.haskell.org/ghcup/) | A TUI for `ghcup`, the Haskell toolchain manager |
| [`cbookview`](https://github.com/mlang/chessIO) | A TUI for exploring polyglot chess opening book files |
| [`thock`](https://github.com/rmehri01/thock) | A modern TUI typing game featuring online racing against friends |
| [`fifteen`](https://github.com/benjaminselfridge/fifteen) | An implementation of the [15 puzzle](https://en.wikipedia.org/wiki/15_puzzle) |
| [`maze`](https://github.com/benjaminselfridge/maze) | A Brick-based maze game |
| [`pboy`](https://github.com/2mol/pboy) | A tiny PDF organizer |
| [`hyahtzee2`](https://github.com/DamienCassou/hyahtzee2#readme) | Famous Yahtzee dice game |
| [`brewsage`](https://github.com/gerdreiss/brewsage#readme) | A TUI for Homebrew |
| [`sandwich`](https://codedownio.github.io/sandwich/) | A test framework with a TUI interface |
| [`youbrick`](https://github.com/florentc/youbrick) | A feed aggregator and launcher for Youtube channels |
| [`swarm`](https://github.com/byorgey/swarm/) | A 2D programming and resource gathering game |
| [`hledger-ui`](https://github.com/simonmichael/hledger) | A terminal UI for the hledger accounting system. |
| [`hledger-iadd`](http://github.com/rootzlevel/hledger-iadd) | An interactive terminal UI for adding hledger journal entries |

These third-party packages also extend `brick`:

| Project | Description |
| ------- | ----------- |
| [`brick-filetree`](https://github.com/ChrisPenner/brick-filetree) [[Hackage]](http://hackage.haskell.org/package/brick-filetree) | A widget for exploring a directory tree and selecting or flagging files and directories |

Release Announcements / News
----------------------------

Find out about `brick` releases and other news on Twitter:

https://twitter.com/brick_haskell/

Getting Started
---------------

Check out the many demo programs to get a feel for different aspects of
the library:

```
$ cabal new-build -f demos
$ find dist-newstyle -type f -name \*-demo
```

To get started, see the [user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst).

Documentation
-------------

Documentation for `brick` comes in a variety of forms:

* [The official brick user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst)
* [Samuel Tay's brick tutorial](https://github.com/jtdaugherty/brick/blob/master/docs/samtay-tutorial.md)
* Haddock (all modules)
* [Demo programs](https://github.com/jtdaugherty/brick/blob/master/programs) ([Screenshots](https://github.com/jtdaugherty/brick/blob/master/docs/programs-screenshots.md))
* [FAQ](https://github.com/jtdaugherty/brick/blob/master/FAQ.md)

Feature Overview
----------------

`brick` comes with a bunch of batteries included:

 * Vertical and horizontal box layout widgets
 * Basic single- and multi-line text editor widgets
 * List and table widgets
 * Progress bar widget
 * Simple dialog box widget
 * Border-drawing widgets (put borders around or in between things)
 * Generic scrollable viewports and viewport scroll bars
 * General-purpose layout control combinators
 * Extensible widget-building API
 * User-customizable attribute themes
 * Type-safe, validated input form API (see the `Brick.Forms` module)
 * A filesystem browser for file and directory selection
 * Borders can be configured to automatically connect!

Brick-Users Discussion
----------------------

The `brick-users` Google Group / e-mail list is a place to discuss
library changes, give feedback, and ask questions. You can subscribe at:

[https://groups.google.com/group/brick-users](https://groups.google.com/group/brick-users)

Status
------

There are some places were I have deliberately chosen to worry about
performance later for the sake of spending more time on the design
(and to wait on performance issues to arise first). `brick` is also
something of an experimental project of mine and some aspects of the
design involve trade-offs that might not be right for your application.
Brick is not intended to be all things to all people; rather, I want it
to provide a good foundation for building complex terminal interfaces
in a declarative style to take away specific headaches of building,
modifying, and working with such interfaces, all while seeing how far we
can get with a pure function to specify the interface.

`brick` exports an extension API that makes it possible to make your own
packages and widgets. If you use that, you'll also be helping to test
whether the exported interface is usable and complete!

Reporting bugs
--------------

Please file bug reports as GitHub issues.  For best results:

 - Include the versions of relevant software packages: your terminal
   emulator, `brick`, `ghc`, and `vty` will be the most important
   ones.

 - Clearly describe the behavior you expected ...

 - ... and include a minimal demonstration program that exhibits the
   behavior you actually observed.

Contributing
------------

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you want to take on big things, talk to me first; let's have a
   design/vision discussion before you start coding. Create a GitHub
   issue and we can use that as the place to hash things out.
 - Please make changes consistent with the conventions I've used in the
   codebase.
 - Please adjust or provide Haddock and/or user guide documentation
   relevant to any changes you make.
 - New commits should be `-Wall` clean.
 - Please do NOT include package version changes in your patches.
   Package version changes are only done at release time when the full
   scope of a release's changes can be evaluated to determine the
   appropriate version change.
