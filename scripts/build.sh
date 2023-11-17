#!/bin/sh

stack build 2>&1 | tee build.log
