#!/bin/bash
# pm addon checks against an HTTP worker (persistent_minaj) started here on <tree> with the store $MINAJ_STORE.
# Usage (from the package root): bash tests/persistent/test_pm_addon.sh <GCAM tree> <scratch folder>
#   Uses electricity.xml and energy/A23.globaltech_capacity_factor (GCAM 9.1 names). The worker runs in <scratch folder>
#   (relative dest= is read from there). Needs MINAJ_STORE, curl, and GNU stat; MINAJ_HOME as in the R tests.
TREE=$1; SCR=$2
[ -d "$TREE/input/gcamdata" ] && [ -n "$SCR" ] || { echo "usage: bash tests/persistent/test_pm_addon.sh <GCAM tree> <scratch folder>" >&2; exit 2; }
[ -n "$MINAJ_STORE" ] || { echo "set MINAJ_STORE to the store the worker uses" >&2; exit 2; }
PM="$(pwd)/inst/bin/pm"; [ -f "$PM" ] || { echo "run from the minaj package root" >&2; exit 2; }
RS="${R_HOME:+$R_HOME/bin/}Rscript"
export PM_INFO="$MINAJ_STORE/server/current.json"
mkdir -p "$SCR"; cd "$SCR" || exit 2
W=$(pwd -W 2>/dev/null || pwd); D=$W/pm_addon_abs; cf=energy/A23.globaltech_capacity_factor
if [ -n "$MINAJ_HOME" ]; then LOAD="suppressMessages(pkgload::load_all('$MINAJ_HOME', quiet = TRUE))"; else LOAD="library(minaj)"; fi
t0=$(date +%s); rm -f worker.log
"$RS" -e "$LOAD; minaj::persistent_minaj('$TREE', store = '$MINAJ_STORE')" > worker.log 2>&1 &
for i in $(seq 1 120); do [ -f "$PM_INFO" ] && [ $(stat -c %Y "$PM_INFO") -ge $t0 ] && bash "$PM" status > /dev/null 2>&1 && break; sleep 2; done
pass=0; fail=0
chk() { local label=$1 pat=$2; shift 2; local out; out=$(bash "$PM" "$@" 2>&1)
        if echo "$out" | grep -qE -- "$pat"; then pass=$((pass+1)); echo "PASS $label"; else fail=$((fail+1)); echo "FAIL $label"; echo "$out" | head -3 | cut -c1-300; fi; }
chk "pm help lists addon" "pm addon <xml>" help
chk "pm addon x1.1, absolute dest" 'electricity_addon.xml (written|unchanged) \(universal add-on\); add directly after the base file: <Value name=\\"electricity_addon\\">' addon electricity.xml $cf=1.1 dest=$D
chk "pm addon x1.1 again: unchanged" 'electricity_addon.xml unchanged' addon electricity.xml $cf=1.1 dest=$D
chk "pm addon, relative dest from the worker's folder" '"ok":true' addon electricity.xml $cf=1.1 dest=pm_addon_rel
[ -f "pm_addon_rel/electricity_addon.xml" ] && { pass=$((pass+1)); echo "PASS   file in <worker folder>/pm_addon_rel"; } || { fail=$((fail+1)); echo "FAIL   file in <worker folder>/pm_addon_rel"; }
chk "pm try x1.1" 'sandbox active' try $cf=1.1
chk "pm addon from the sandbox: unchanged" 'electricity_addon.xml unchanged.*"sandbox":\[?"energy/A23' addon electricity.xml dest=$D
chk "pm summary line: the configuration line unescaped" '^electricity_addon.xml unchanged .*<Value name="electricity_addon">[^\\]*</Value>$' addon electricity.xml $cf=1.1 dest=$D
chk "pm try clear" 'sandbox empty' try clear=true
chk "pm addon, unknown input: error" 'not a CSV input or data object' addon electricity.xml energy/no_such_input=1.1
chk "pm addon, config missing: error" '"ok":false' addon electricity.xml $cf=1.1 config=$W/no_such_config.xml
chk "pm addon x1.2 with confirm=true: written" 'electricity_addon.xml written' addon electricity.xml $cf=1.2 dest=$D confirm=true
bash "$PM" stop > /dev/null; wait
echo "RESULT pm addon: PASS $pass FAIL $fail"
[ $fail -eq 0 ]
