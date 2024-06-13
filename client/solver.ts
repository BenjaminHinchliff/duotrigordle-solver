import assert from "assert";
import type { Browser, Page } from "puppeteer";
import puppeteer from "puppeteer";
import swipl from "swipl";
import { term } from "swipl";
// import { WORDS_TARGET } from "./wordlist";
const { list, compound, variable, serialize } = term;

const CHANGELOG_CLOSE_SELECTOR = "._modal_y9oz3_1 > ._button_1xh0d_1";
const PRACTICE_TAB_SELECTOR = "div._tabs_5us1c_1 > div:nth-child(2) > button";
const PRACTICE_LINK_SELECTOR =
  "div._tabWrapper_ag3fe_24 > div._tabContainer_ag3fe_30._shown_ag3fe_36 > div:nth-child(1) > button";
const MAIN_SELECTOR = "._main_kv0wd_1";
const BOARDS_SELECTOR = "div._board_1277w_1";
const CELL_SELECTOR = "div._cell_1277w_56";
const RESULTS_SELECTOR = "div._resultsContainer_10b87_1";

const KEEP_PLAYING_SELECTOR =
  "#root > div > div._modalWrapper_y9oz3_1._lightweight_y9oz3_37 > div._modal_y9oz3_1 > div > button:nth-child(1)";

const EXPORT_SELECTOR = "div._exportContainer_w8urc_154 > button";
const EXPORT_TEXT_SELECTOR = "div._exportContainer_w8urc_154 > textarea";

const STARTER_WORDS = ["RAISE"];
// const STARTER_WORDS = ["ABHOR", "CITED", "FLUNG", "JUMPY", "ASKEW"];
// const STARTER_WORDS = ["FJORD", "SWUNG", "PICKY", "AMPLY", "HERTZ"];

async function gather_results(page: Page): Promise<string[]> {
  const boards = await page.$$(BOARDS_SELECTOR);
  const resultsPromises = boards.map(async (board) => {
    const boardCls = await board.evaluate((el) => el.className);
    if (boardCls.includes("dimmed")) {
      return "fffff";
    }

    const cells = await board.$$eval(CELL_SELECTOR, (els) =>
      els.map((el) => el.className)
    );
    const result = cells.slice(-10, -5);
    return result
      .map((cls) =>
        cls.includes("green") ? "f" : cls.includes("yellow") ? "p" : "i"
      )
      .join("");
  });

  const results = await Promise.all(resultsPromises);
  assert(
    results.length === 32,
    `Expected 32 boards, but found ${results.length}`
  );
  return results;
}

export class DuotrigordleSolver {
  headless: boolean
  browser: Browser | null
  page: Page | null

  constructor(headless: boolean) {
    this.headless = headless;
  }

  async setup(): Promise<void> {
    // Launch the browser and open a new blank page
    this.browser = await puppeteer.launch({ headless: this.headless });

    const page = this.page = await this.browser.newPage();

    // Navigate the page to a URL
    await page.goto("https://duotrigordle.com/game/practice/normal");

    await page.waitForSelector(CHANGELOG_CLOSE_SELECTOR);
    await page.click(CHANGELOG_CLOSE_SELECTOR);
    await page.click(PRACTICE_TAB_SELECTOR);

    await page.waitForSelector(PRACTICE_LINK_SELECTOR);
    // for whatever reason the events take a sec to get hooked up
    await new Promise((resolve) => setTimeout(resolve, 500));
    await page.click(PRACTICE_LINK_SELECTOR);

    swipl.call("['../entropy/entropy']");
  }

  async reset(): Promise<void> {
    await this.page.keyboard.down("Control")
    await this.page.keyboard.press("KeyR")
    await this.page.keyboard.up("Control")
  }

  async solveDuotrigordle(): Promise<void> {
    if (!(this.browser || this.page)) {
      return
    }

    const { page } = this

    const guesses: string[] = [];
    const all_results: string[][] = Array(32)
      .fill(null)
      .map((_) => []);
    for (const starter of STARTER_WORDS) {
      await page.type(MAIN_SELECTOR, starter);
      await page.keyboard.press("Enter");

      guesses.push(starter);
      const results = await gather_results(page);
      all_results.forEach((acc, i) => acc.push(results[i]));
    }

    while (!(await page.$(RESULTS_SELECTOR))) {
      if (await page.$(KEEP_PLAYING_SELECTOR)) {
        await page.click(KEEP_PLAYING_SELECTOR);
      }

      const query = serialize(
        compound("best_word_given", [
          list(guesses),
          list(all_results.map(list)),
          variable("ME"),
        ])
      );
      console.info("querying prolog:", query);
      const ret = swipl.call(query);
      console.info("result:", ret.ME);

      if (!ret) {
        console.error("prolog query failed!");
        break;
      }

      await page.type(MAIN_SELECTOR, ret.ME);
      await page.keyboard.press("Enter");

      // await new Promise((resolve) => setTimeout(resolve, 1000));

      guesses.push(ret.ME);

      const results = await gather_results(page);
      all_results.map((acc, i) => acc.push(results[i]));
    }

    console.info("SOLVED.");
  }

  async collectStats() {
    await this.page.goto("https://duotrigordle.com/stats/practice/normal");

    await this.page.click(EXPORT_SELECTOR);
    return await this.page.$eval(EXPORT_TEXT_SELECTOR, (el) => el.value);
  }

  async cleanup() {
    await this.browser.close()
  }
}
