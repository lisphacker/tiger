#!/bin/sh

stack test 2>&1 | tee test.log
