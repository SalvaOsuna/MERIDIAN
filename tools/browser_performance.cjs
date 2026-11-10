const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const fs = require('fs');
const path = require('path');
const out = path.resolve('validation-output/performance');
(async () => {
  const browser = await chromium.launch({channel: 'msedge', headless: true});
  const page = await browser.newPage({viewport: {width: 1600, height: 1100}, acceptDownloads: true});
  page.setDefaultTimeout(90000);
  const idle = async () => {
    await page.waitForTimeout(350);
    await page.waitForFunction(() => window.Shiny && !document.documentElement.classList.contains('shiny-busy') && !Array.from(document.querySelectorAll('.recalculating')).some(el => el.getClientRects().length));
    const errors = await page.locator('.shiny-output-error:visible,.shiny-notification-error:visible').allTextContents();
    if (errors.length) throw Error(errors.join('; '));
  };
  const select = async (id, value) => {
    await page.locator('#'+id).evaluate((el,v) => {
      if(el.selectize) el.selectize.setValue(v);
      else window.jQuery(el).val(v).trigger('change');
    }, value);
    await idle();
  };
  const timings = [];
  const timed = async (label, action) => {
    const start = Date.now(); await action(); await idle();
    timings.push({stage: label, seconds: (Date.now()-start)/1000});
    console.log(timings[timings.length-1]);
  };
  await page.goto('http://127.0.0.1:3876');
  await page.locator('#btn_upload_own').click();
  await page.locator('#data-file_upload').setInputFiles(path.join(out,'user-sized-example.csv'));
  await page.waitForFunction(() => document.querySelector('#data-col_gen')?.value === 'gen');
  await idle();
  await select('data-col_traits', ['y', ...Array.from({length:8},(_,i)=>'trait'+(i+2))]);
  console.log('DATA LOADED');
  await page.locator('a[data-value="tab_anova"]').click();
  await timed('first_trait_fit_and_render', async () => {
    await page.locator('#anova-run_anova').click();
    await page.waitForFunction(() => document.querySelector('#anova-anova_table')?.textContent.includes('Genotype (G)'));
  });
  await timed('repeat_same_analysis', () => page.locator('#anova-run_anova').click());
  await timed('reverse_selection_direction', () => select('anova-direction','lower'));
  await select('anova-trait','trait2');
  await timed('second_trait_fit_and_render', async () => {
    await page.locator('#anova-run_anova').click();
    await page.waitForFunction(() => document.querySelector('#anova-anova_table')?.textContent.includes('Genotype (G)'));
  });
  await select('anova-trait','y');
  await timed('return_to_cached_trait', () => page.locator('#anova-run_anova').click());
  await page.locator('#anova-send_anova_table_report').click();
  await page.locator('#anova-send_ss_barplot_report').click();
  await page.locator('a[data-value="tab_reports"]').click(); await idle();
  for (const [id, filename] of [['reports-dl_table_csv','large-anova.csv'],['reports-dl_table_xlsx','large-anova.xlsx']]) {
    const pending = page.waitForEvent('download'); await page.locator('#'+id).click();
    const file = await pending; await file.saveAs(path.join(out,filename));
    if(fs.statSync(path.join(out,filename)).size < 100) throw Error('Empty '+filename);
  }
  await page.getByRole('tab',{name:'Registered Plots',exact:true}).click();
  await select('reports-plt_format','PNG');
  const pending = page.waitForEvent('download'); await page.locator('#reports-dl_plot_file').click();
  await (await pending).saveAs(path.join(out,'large-anova.png'));
  await idle();
  fs.writeFileSync(path.join(out,'browser-timings.json'),JSON.stringify(timings,null,2));
  await browser.close();
})().catch(e => { console.error(e); process.exit(1); });
