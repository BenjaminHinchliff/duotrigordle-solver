import { writeFileSync } from 'fs'
import { DuotrigordleSolver } from "./solver"

(async () => {
  const RUNS = 50;

  const solver = new DuotrigordleSolver(true);
  await solver.setup();

  for (let i = 0; i < RUNS; ++i) {
    await solver.solveDuotrigordle();
    await solver.reset();
  }
  await solver.cleanup();

  const stats = await solver.collectStats();
  writeFileSync("stats.csv", stats);
})();
