# Interactive HTML BOM plugin for Altium Designer
## Supports Altium Designer

This plugin inspired with [InteractiveHtmlBomForAD](https://github.com/lianlian33/InteractiveHtmlBomForAD), but (i hope) less of bugs and more of features.

Plugin is ready to use as OutJob Report Output. Based on plugin for KiCad [InteractiveHtmlBom](https://github.com/openscopeproject/InteractiveHtmlBom).

This plugin generates a convenient Bill of Materials (BOM) listing with the
ability to visually correlate and easily search for components and their placements
on the PCB. It is particularly useful when hand-soldering a prototype, as it allows
users to quickly find locations of components groups on the board. It is also possible
to reverse lookup the component group by clicking on a footprint on the board drawing.

The plugin utilizes Altium Designer pascal script and API to read Project/SCH/PCB data and render silkscreen,
footprint pads, text, and drawings.

There is an option to include tracks/zones data as well as netlist information allowing
dynamic highlight of nets on the board.

Generated html page is fully self contained, doesn't need internet connection to work
and can be packaged with documentation of your project or hosted anywhere on the web.

## Installation and Usage

WIP.

## How to use
_Step 1_: download the add-on you want.
1. Either clone the entire set of add-ons on your computer (if you're accustomed to Git),
2. Or download only the add-on you want. You can do so by navigating into a script folder and then clicking the script link from the list.

_Step 2_: integrate the script into Altium Designer and execute it.\
If you are a newcomer to Altium scripts, [please read the "how to" help page](https://github.com/Altium-Designer-addons/scripts-libraries/blob/master/HowTo_execute_scripts.md). Some developers may also have provided some screen captures or examples of use.

## License and credits

WIP.
