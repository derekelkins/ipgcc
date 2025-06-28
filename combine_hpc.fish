#!/usr/bin/env fish

set BASE_DIR (string split -f 1 '/x/' (cabal list-bin ipgcc))

set OUT_DIR "./hpc"

if test ! -d "$OUT_DIR"; mkdir $OUT_DIR; end

set COMBINED_TIX "$OUT_DIR/combined.tix"

# TODO: It would be nice not to have to list these.
set MODULES Text.IPG.Check
set -a MODULES Text.IPG.Core
set -a MODULES Text.IPG.Export.JS
set -a MODULES Text.IPG.Full
set -a MODULES Text.IPG.GenericExp
set -a MODULES Text.IPG.Interpreter
set -a MODULES Text.IPG.Lexer
set -a MODULES Text.IPG.Parser
set -a MODULES Text.IPG.PPrint
set -a MODULES Text.IPG.Simple
set -a MODULES Text.IPG.TopLevel.FileSplit
for m in $MODULES; set -a INCLUDED_MODULES "--include=$m"; end

set HPC_DIR "$BASE_DIR/build/extra-compilation-artifacts/hpc/vanilla/mix"

set EXPORT_TIX "$BASE_DIR/t/export-test/hpc/vanilla/tix/export-test.tix"
set NODE_TIX "$BASE_DIR/t/node-test/hpc/vanilla/tix/node-test.tix"
set PARSING_TIX "$BASE_DIR/t/parsing-test/hpc/vanilla/tix/parsing-test.tix"
set INTERPRETER_TIX "$BASE_DIR/t/interpreter-test/hpc/vanilla/tix/interpreter-test.tix"

hpc combine --union $EXPORT_TIX $NODE_TIX --output $COMBINED_TIX
hpc combine --union $COMBINED_TIX $PARSING_TIX --output $COMBINED_TIX
hpc combine --union $COMBINED_TIX $INTERPRETER_TIX --output $COMBINED_TIX
hpc markup --destdir $OUT_DIR --hpcdir=$HPC_DIR $COMBINED_TIX $INCLUDED_MODULES
