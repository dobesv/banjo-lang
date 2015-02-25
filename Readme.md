
Banjo is an experimental programming language for cross-platform development, particularly for video games.

It's not unlike Monkey, SkookumScript, or haXe in its intended use, but with a different programming language design.

The design goals of this programming language are:

  1. Great IDE support, for example:
    * Error checking as you type
    * Name completion (CTRL+SPACE)
    * Jump to definition
    * Refactoring
  2. Output to multiple target languages/platforms
    * JS/HTML5 for simple web / 2D games
    * C++ backend for performance intensive games
  3. Dynamic programming
    * Parser / compiler / interpreter as part of the standard library
    * Compiled code with interpreter hooks so "native" functions are accessible from the interpreter
    * Homoiconic language - compile/load time self-modifying code
    * Live code reload - modify the game in front of your eyes
  4. Modularity and code re-use
    * Compose modules, objects, and functions in different ways
    * Row polymorphism
    * Structural typing

The degree to which these goals are achieved is still to be determined, and these goals are still subject to change without notice.

