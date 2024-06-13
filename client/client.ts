import { DuotrigordleSolver } from './solver'

(async () => {
  const solver = new DuotrigordleSolver(false)
  await solver.setup()
  await solver.solveDuotrigordle()
  await solver.cleanup()
})();
