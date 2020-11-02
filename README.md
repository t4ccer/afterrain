# afterrain
  After rain comes rainbow, and with rainbow come colors. Add some colors to your terminal with `afterrain`.

## Instalation
### Requriments: 
 - Stack

  ```bash
  git clone https://github.com/t4ccer/afterrain
  cd afterrain
  stack build --copy-bins
  ```

## Usage
  ### Hoogle output highlighting:
  ```bash
  hoogle <command> | aft hoogle
  ```
  OR  
  
  
  Add this to your .bashrc
  ```bash
  hs(){
    hoogle "$@" | aft hoogle
  }
  ```
  ```
  hs <command>
  ```

## Configuration
  Config file is located in `$HOME/.afterrain.yaml`, and is created automaticly after first run.

## Suported features
 - Hoogle highlighting

## Planned features
 - More programs support
