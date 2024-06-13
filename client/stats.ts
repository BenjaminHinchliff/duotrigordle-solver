import { writeFileSync } from 'fs'
import { argv, pid } from 'process'
import { DuotrigordleSolver } from "./solver"

(async () => {
  const RUNS = 50;

  const starters = argv.slice(2);

  console.log(`getting stats for ${pid}`)
  console.log(`starters: ${starters}`)

  const solver = new DuotrigordleSolver(starters, true);
  await solver.setup();

  for (let i = 0; i < RUNS; ++i) {
    await solver.solveDuotrigordle();
    await solver.reset();
  }

  const stats = await solver.collectStats();
  writeFileSync(`stats-${pid}.csv`, stats);

  await solver.cleanup();
})();
