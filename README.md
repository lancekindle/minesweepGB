#Minesweeper
#(For the Gameboy)
![](/home/lance/code/asm/class_code/minesweep/screenshot.png) 

Built mostly ground up in assembly, my take on the classic game of minesweeper.
I followed the excellent ["hello world" examples](http://cratel.wichita.edu/cratel/ECE238Spr08) by Witchita State University. These begin with displaying a set of tiles on the Gameboy's screen and include moving a character on the screen and working with noise / music. They also include an introduction (by example) of RGBDS's asm and macro syntax. To get these examples to work I had to merge hardware.inc with a version of gbhw.inc found online. This is mostly due to some missing older defined constants (and maybe the ROM_HEADER macro) within hardware.inc.

#Features
###Macros
Freakin' awesome macros. I've seen it said that RGBDS has amazing macro support. And it's true, but its syntax is very terse. Minesweep contains some powerful macros that make reading assembly MUCH easier. Check out syntax.asm to see some excellent macros that make readable assembly. My Favorite: IFA
```
ifa	>, 32,	ld a, 32
```
Which can be read as "IF A > 32, load A with 32"

IFA is used (in this case) to run this psuedo-code: 
```a = min(a, 32)```.
In assembly, above macro expands to:
```
cp	32
jr	nc, .skip
ld	a, 32
.skip
```
###Responsive, silky-smooth gameplay
Minesweep uses hardware interrupts and careful state management to number crunch and reveal areas on the minefield without sacrificing player-input response time. The effect is split-second minefield reveal with no lag to keypresses. It's as multi-threaded as possible on a single-core cpu. The cursor remains responsive throughout and features smooth scrolling between locations.