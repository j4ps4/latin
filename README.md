## latin dictionary

This is a simple commandline latin dictionary. It understands declension of verbs and nouns and can output full declension tables.

## build & install

Building the project requires stack (https://www.haskellstack.org/) and sqlite libraries and headers.

1. ```bash
   git clone "https://www.github.com/j4ps4/latin" && cd latin
   
   ```

2. ```bash
   stack build
   ```

3. ```bash
   stack install
   ```

## usage

The program requires the database file latin.db to be in the current directory, or the full path to the database file is read from \$HOME/.latin.conf

```bash
# output basic info
latin amo
# understands declined words
latin amavisse
# output full declension
latin -v amo
```
