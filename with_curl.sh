#!/bin/bash

FILE_LINK="https://psv4.userapi.com/s/v1/d2/enqaXfcs1B0uZ2bXY-tXakPVForrLpQI6Nc09cEaerGo-q3fzStby9LuLu78K8FxboVsM5Z92hdbvo3YPlzHPM3v1CzJj3_UQf2awiNHR1heDstwCpGoxJkz4QHcsgN-lCfu3a_m2zng/input.txt"

curl -L $FILE_LINK | cabal run rel -- Ерофей
