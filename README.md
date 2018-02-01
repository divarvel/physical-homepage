# Physical homepage

## Run

    stack setup
    stack install
    ./spock-exe

## Develop

    npm install -g devloop # if not already available
    loop

## Troubleshoot on MacOS

If the compilation fails with a message about missing ICU libraries / headers:

    brew install icu4c
    stack install text-icu \
        --extra-lib-dirs=/usr/local/opt/icu4c/lib \
        --extra-include-dirs=/usr/local/opt/icu4c/include

## How it works

Talks are fetched from evman at boot time.
