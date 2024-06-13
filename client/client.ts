import { DuotrigordleSolver } from './solver'

(async () => {
  const solver = new DuotrigordleSolver(["RAISE"], false)
  await solver.setup()
  await solver.solveDuotrigordle()
  await solver.cleanup()
})();
