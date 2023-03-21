# Mach-O and DWARF parser for Scala (ELF/COFF coming soon)

The intention of this project is to eventually end up in Scala Native runtime sources,
as a complement to https://github.com/scala-native/scala-native/pull/2869 - 
which would allow us to enrich the exceptions with file name and line number.

It's a Scala CLI project, so you can just do `scala-cli run -- <binary>` and 
if that binary contains DWARF metadata and if we can parse it - you will see some output.

Contributors:

- DWARF4 format: https://dwarfstd.org/doc/DWARF4.pdf
- LIEF library - useful for interactively exploring file formats: https://lief-project.github.io/doc/latest/getting_started.html
- dwex: DWARF explorer GUI: https://github.com/sevaa/dwex
