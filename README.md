# Duotrigordle Solver
#### Prepared for 2024 CSC481 under Dr. Rodrigo Canaan

## Client usage

Should start an instance of Puputeer-driven Chromium, with the Prolog solver
running in swi-prolog via the `swipl` bridge npm library. 

```shell
cd client
npm install
npm run start
```

## Game statistics Gathering Usage

```shell
cd client
# takes as aruments the starter words to test with
npx ts-node stats.ts RAISE
```

## Visualizations

Visualziations can be found under the `visualization` subdirectory, along with
the notebook used to create them.

## Letter Maximization Usage

```shell
cd letter_maximization
make
./letter_maximization
```

## Greedy Letter Entropy Maximization Usage

```shell
cd entropy_greedy
python entropy_greedy.py
```
