#!/usr/bin/env nu
# export-nix-to-md.nu
# Erzeugt nix-files.md mit allen .nix-Dateien als ```nix```-Blöcken.

# Zieldatei initialisieren (überschreiben, falls vorhanden)
"# Sammlung der .nix-Dateien\n\n" | save nix-files.md -f

# Alle .nix Dateien rekursiv finden, nach Name sortieren und nacheinander anhängen
let files = (ls *.nix | sort-by name)

for file in $files {
    let path = $file.name
    let name = $file.name

    # Dateiinhalt als einzelne string-Variable zusammenfügen
    let content = (open $path | lines | str join "\n")

    # Markdown-Block erzeugen (klassisch per String-Konkatenation)
    let header = "### " + $name + "\n\n"
    let opening = "```nix\n"
    let closing = "\n```\n\n"

    # Alles zusammensetzen und anhängen
    ($header + $opening + $content + $closing) | save -a nix-files.md

    
    let base = ($name | str replace ".nix" "") + *.exp
    let matches = (glob $base)

    if ( ($matches | length) > 0 ) {
        let err_path = ($matches | get 0)
        print $"Found err file: ($err_path)"

        let content = (open $err_path | to text)

        let err_block = ("#### Expected \n\n"
             + "```text\n"
             + $content
             + "\n```\n\n")

        $err_block | save -a nix-files.md
    }        
}

