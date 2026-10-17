#!/bin/bash
# The three edit tests driven through persistent_minaj (pm calls against a running worker):
# edit a file, `pm make`, `pm diff` to check the edit did what was meant, switch back. Plus a sandbox trial.
# Every timing is the full client call (bash + curl + worker). Files are restored byte for byte.
# Needs bash with GNU date and md5sum (Linux or Git Bash on Windows).
# Usage (from the package root): bash tests/persistent/run_pm_tests.sh <tree> > tests/persistent/pm_tests.log
#   (last of make_csvs.R -> drake_insession.R -> minaj_insession.R -> run_pm_tests.sh; perl, cmp and curl;
#   a worker must be running on the same tree and store:
#   Rscript inst/bin/persistent_minaj.R <tree> [store]   (store = $MINAJ_STORE when MINAJ_STORE is set).
#   pm reads <store>/server/current.json for MINAJ_STORE if set, else for the default store; PM_INFO overrides.)
[ -d "$1/input/gcamdata" ] || { echo "usage: bash tests/persistent/run_pm_tests.sh <GCAM tree>" >&2; exit 1; }
if [ -n "$MINAJ_STORE" ]; then export PM_INFO="${PM_INFO:-$MINAJ_STORE/server/current.json}"; fi
T=$(cd "$(dirname "$0")/.." && pwd); PM=$T/../inst/bin/pm; G=$(cd "$1" && pwd)/input/gcamdata
TW=$(cd "$T" && { pwd -W 2>/dev/null || pwd; }); BAK=$(mktemp); trap 'rm -f "$BAK"' EXIT
bash "$PM" status > /dev/null || { echo "no persistent_minaj worker reachable; start one first (see the header)" >&2; exit 1; }
run() { local s e out; s=$(date +%s%N); out=$(bash "$PM" "$@" --raw); e=$(date +%s%N)
        printf '%-58s %7.1f s  %s\n' "pm $(echo "$*" | cut -c1-52)" "$(awk -v a="$s" -v b="$e" 'BEGIN{printf "%.1f", (b-a)/1e9}')" "$(echo "$out" | sed -n 's/^{"ok":[a-z]*,"summary":"\([^"]*\)".*/\1/p' | cut -c1-150)"; }
INC="socioeconomics_incelas_ssp1.xml socioeconomics_incelas_ssp2.xml socioeconomics_incelas_ssp3.xml socioeconomics_incelas_ssp4.xml socioeconomics_incelas_ssp5.xml"
SOC="socioeconomics_SSP1.xml socioeconomics_SSP2.xml socioeconomics_SSP3.xml socioeconomics_SSP4.xml socioeconomics_SSP5.xml socioeconomics_CORE.xml"

echo "== 1. income elasticity 1.12 -> 1.23 (A32.inc_elas_output; a value not used before)"
CSV=$G/inst/extdata/socioeconomics/A32.inc_elas_output.csv; cp -p "$CSV" "$BAK" || exit 1
trap 'cmp -s "$BAK" "$CSV" || cp -p "$BAK" "$CSV" && rm -f "$BAK"' EXIT
run make $INC
perl -pi -e 's/^3,1\.12(\r?)$/3,1.23$1/' "$CSV"
run make $INC
run diff socioeconomics_incelas_ssp2.xml
cp -p "$BAK" "$CSV"; trap 'rm -f "$BAK"' EXIT
run make $INC

echo "== 2. constants.R: socioeconomics.GDP_DIGITS 0 -> 2 (a value not used before)"
F=$G/R/constants.R; cp -p "$F" "$BAK" || exit 1
trap 'cmp -s "$BAK" "$F" || cp -p "$BAK" "$F" && rm -f "$BAK"' EXIT
run make $SOC
perl -pi -e 's/^(socioeconomics\.GDP_DIGITS +<- +)0\b/${1}2/' "$F"
run make $SOC
run diff socioeconomics_SSP2.xml
cp -p "$BAK" "$F"; trap 'rm -f "$BAK"' EXIT
run make $SOC

echo "== 3. PV capacity factor low / mid / high (same three CSV files as the drake test)"
B="{\"xml\":\"electricity.xml\",\"variants\":{\"low\":{\"energy/A23.globaltech_capacity_factor\":\"$TW/pv_hml/inputs/A23_low.csv\"},\"mid\":{\"energy/A23.globaltech_capacity_factor\":\"$TW/pv_hml/inputs/A23_mid.csv\"},\"high\":{\"energy/A23.globaltech_capacity_factor\":\"$TW/pv_hml/inputs/A23_high.csv\"}}}"
run variants --body "$B"
run variants --body "$B"
for v in low mid high; do echo "   $v: same file as drake's: $(cmp -s "$T/pv_hml/out/drake/electricity_$v.xml" "$G/xml_minaj/variants/electricity_$v.xml" && echo yes || echo NO)"; done

echo "== 4. sandbox: PV future capacity factor x1.1 without touching any file"
S='{"set":{"energy/A23.globaltech_capacity_factor":[{"op":"scale","value":"1.1","technology":"PV;PV_storage","column":"initial-future-year;2050;2100"}]}}'
run try --body "$S"
run make electricity.xml
run diff L223.StubTechCapFactor_elec against=base
run try clear=true
git -C "$G/../.." status --short | head -3
