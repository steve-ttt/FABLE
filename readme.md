
# F.A.B.L.E. - Fiction and Adventure Building Lisp Engine



# Text-Based Adventure Game Engine

This is a simple text-based adventure game implemented in Common Lisp. The game allows the player to move through different locations, pick up items, and inspect their surroundings.

## History of Text Adventure Games and Lisp's Involvement

Text adventure games, also known as interactive fiction, emerged in the late 1970s. These pioneering games relied solely on text-based descriptions, prompting players to type commands to navigate through the game world. They reached their peak in popularity in the late 1970s and 1980s, with notable titles like Zork and The Hitchhiker's Guide to the Galaxy by Infocom.

The history of text-based games began in 1971 with a programmer, cave explorer, and D&D (Dungeons and Dragons) player William Crowther. Crowther’s passion for spelunking and a romance with tabletop RPGs became the inspiration for the first text-based adventure game, Adventure.

Lisp, due to its expressive syntax and powerful features, has been used to create text adventure games. 
ZIL, short for Zork Implementation Language, is a programming language developed by Infocom and is based on MDL. MDL, also known as Muddle, is a version of Lisp created by MIT students and staff. ZIL was specifically designed for creating text adventure games like Zork. By leveraging the expressive power of Lisp, ZIL allowed game developers to create complex, interactive narratives.


## Structures

The game uses two main structures: `location` and `item`.

### Location

A `location` represents a place in the game world. It has a name, a short description, a long description, a list of exits to other locations, and an inventory of items that are currently in that location.

### Item

An `item` represents an object that the player can interact with. It has a name, a short description, a long description, a current location, an inventory (for items that can contain other items), and a list of flags that represent the item's current state.

## Functions

The game includes several functions that allow the player to interact with the game world:

- `add-location`: Adds a new location to the game world.
- `add-item`: Adds a new item to the game world.
- `look`: Prints a description of the player's current location or a specific object.
- `find-node`: Returns a location node if it appears in the location list.
- `move`: Moves the player in a given direction, if it's a valid exit.
- `take`: Allows the player to take an item from their current location and add it to their inventory.
- `inventory`: Prints the player's current inventory.
- `where`: Prints the player's current location.
- `help`: Prints the documentation string for the given command
- `Save/Load Game State`: Allow players to save their progress and load it later.

## TODO

- `Rideable opjects`: Implement a system where you can ride or sit in or on objects, cars, boats, chairs, etc
- `NPC Interactions`: Implement non-player characters (NPCs) that the player can interact with.
- `Puzzles`: Add puzzles that the player must solve to progress.
- `Combat System`: Possibility of adding a simple combat system (insult sword fighting springs to mind).
- `Detailed Item Descriptions`: Expand on the item descriptions, perhaps adding history or lore to each item.
- `Item Interactions`: Allow items to interact with each other in the player’s inventory or the environment.
- `Dynamic Environment`: Make the game world dynamic, changing based on player actions or over time.


## Dependancies

The game requires Common LISP with quicklisp to build but the executable will work standalone after compiled. It was build and tested on SBCL but any version of Common LISP should work (with some tweaking). Common LISP is available on LINUX, MacOS, and windows.


## Usage

To play the game, load the file in your Lisp environment and start interacting with the game world by calling the functions with the appropriate arguments.

## How to Compile

The easiest way is to run the makefile (assuming your have sbcl installed)
```easy way
make
```

To compile the game in the repl, use the following command (assuming SBCL):

```lisp
(sb-ext:save-lisp-and-die "cool-game-name.exe" :executable t :toplevel 'start)
```

Then, you can run the game with the following command:

```bash
./myAdventure
```