#!/usr/bin/env bash

kubectl config current-context | grep -Eo '\-[a-z]*\.[a-z]*' | tr -d "-"
