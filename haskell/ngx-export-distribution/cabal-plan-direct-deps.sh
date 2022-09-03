#!/usr/bin/env bash

CABAL_PLAN=$(cabal-plan info --ascii)
UNIT_ID="^UnitId\s\+\""
while IFS= read -r pkg
do sed -n "/$UNIT_ID$pkg/s/$UNIT_ID\(.*\)\"\$/package-id \1/p" <<< "$CABAL_PLAN"
done < <(sed -n '/^CompNameLib$/,/^$/s/^\s\+//p' <<< "$CABAL_PLAN")
unset CABAL_PLAN UNIT_ID

