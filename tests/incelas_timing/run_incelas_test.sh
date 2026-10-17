#!/bin/bash
# One income-elasticity edit, timed with driver_drake(stop_after) and driver_minaj(stop_after) on a GCAM 9.1 tree, then switched back.
# Needs bash with GNU date and md5sum (Linux or Git Bash on Windows).
# Usage (from the package root): bash tests/incelas_timing/run_incelas_test.sh <tree> > tests/incelas_timing/incelas_test.log
#   (Rscript and java on PATH; minaj, drake and devtools installed; a tree with a full drake build; perl and cmp;
#   MINAJ_HOME and MINAJ_STORE apply as in run_gcamdata_incelas_minaj.R)
[ -d "$1/input/gcamdata" ] || { echo "usage: bash tests/incelas_timing/run_incelas_test.sh <GCAM tree>" >&2; exit 1; }
RS=Rscript; D=$(cd "$(dirname "$0")" && pwd); G=$(cd "$1" && pwd)/input/gcamdata
CSV=$G/inst/extdata/socioeconomics/A32.inc_elas_output.csv; XCH=module_socio_IncomeElasticity_xml
X="socioeconomics_incelas_ssp1.xml socioeconomics_incelas_ssp2.xml socioeconomics_incelas_ssp3.xml socioeconomics_incelas_ssp4.xml socioeconomics_incelas_ssp5.xml"
md5s() { (cd "$1" && md5sum $X | awk '{print $1}' | tr '\n' ' '); }
BAK=$(mktemp); cp -p "$CSV" "$BAK" || exit 1
trap 'cmp -s "$BAK" "$CSV" || cp -p "$BAK" "$CSV" && rm -f "$BAK"' EXIT
ORIG=$(md5s "$G/xml")
echo "original drake files: $ORIG"
perl -pi -e 's/^3,1\.12(\r?)$/3,1.2$1/' "$CSV"
n=$(grep -c '^3,1\.2.\?$' "$CSV"); echo "edit: $n line changed (industrial income elasticity at 3 GJ/person: 1.12 -> 1.20)"
if [ "$n" != 1 ]; then echo "EDIT FAILED, aborting"; cp -p "$BAK" "$CSV"; exit 1; fi
(cd "$G" && "$RS" "$D/run_gcamdata_incelas_drake.R" edit $XCH 2>&1 | grep RESULT)
"$RS" "$D/run_gcamdata_incelas_minaj.R" edit $XCH "$G" 2>&1 | grep RESULT
A=$(md5s "$G/xml"); B=$(md5s "$G/xml_minaj"); echo "after edit: drake == minaj: $([ "$A" == "$B" ] && echo yes || echo NO); changed from original: $([ "$A" != "$ORIG" ] && echo yes || echo NO)"
cp -p "$BAK" "$CSV"; echo "restored CSV: $(cmp -s "$CSV" "$BAK" && echo byte-identical)"
(cd "$G" && "$RS" "$D/run_gcamdata_incelas_drake.R" switchback $XCH 2>&1 | grep RESULT)
"$RS" "$D/run_gcamdata_incelas_minaj.R" switchback $XCH "$G" 2>&1 | grep RESULT
A=$(md5s "$G/xml"); B=$(md5s "$G/xml_minaj"); echo "after switching back: drake == minaj: $([ "$A" == "$B" ] && echo yes || echo NO); back to original: $([ "$A" == "$ORIG" ] && echo yes || echo NO)"
git -C "$G/../.." status --short | head -3
