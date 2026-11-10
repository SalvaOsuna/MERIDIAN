const {chromium} = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const fs = require('fs');
const path = require('path');
const out = path.resolve('validation-output/browser');
fs.mkdirSync(out,{recursive:true});
(async()=>{
  const browser = await chromium.launch({channel: process.env.BROWSER_CHANNEL || (process.platform==='win32'?'msedge':undefined),headless:true});
  const page = await browser.newPage({viewport:{width:1600,height:1100}});
  page.setDefaultTimeout(60000);
  const idle = async()=>{
    await page.waitForTimeout(300);
    await page.waitForFunction(()=>!document.documentElement.classList.contains('shiny-busy') && !Array.from(document.querySelectorAll('.recalculating')).some(x=>x.getClientRects().length));
    const errors=await page.locator('.shiny-output-error:visible,.shiny-notification-error:visible').allTextContents();
    if(errors.length) throw Error(errors.join('; '));
  };
  const visitTabs=async(value)=>{
    const titles=await page.locator(`div.tab-pane[data-value="${value}"] a[role="tab"]:visible`).allTextContents();
    for(const raw of titles){
      const title=raw.trim();
      await page.locator(`div.tab-pane[data-value="${value}"] a[role="tab"]`).filter({hasText:title}).first().click();
      await idle();
      console.log('TAB OK',value,title);
    }
  };
  await page.goto('http://127.0.0.1:3876');
  await page.locator('#btn_load_example').click();
  await page.waitForFunction(()=>document.querySelector('#data-design_type')?.textContent==='RCBD');
  await page.locator('a[data-value="tab_eda"]').click(); await idle();
  await page.locator('a[role="tab"]').filter({hasText:'Distributions'}).click();
  console.log('DISTRIBUTIONS OPEN');
  await page.locator('#eda-run_distributions').click();
  await idle();
  console.log('DISTRIBUTIONS GENERATED');
  await page.screenshot({path:path.join(out,'07-distributions.png'),fullPage:true});
  await page.waitForFunction(()=>document.querySelector('#eda-distribution_plot img'));
  await page.locator('#eda-send_distribution_report').click();
  await visitTabs('tab_eda');
  for(const [value,button,ready] of [['tab_anova','anova-run_anova','#anova-anova_table'],['tab_stability','stability-run_stability','#stability-plot_ammi1 .plot-container'],['tab_adaptation','adaptation-run_adaptation','#adaptation-plot_fw .plot-container']]) {
    await page.locator(`a[data-value="${value}"]`).click();
    await page.locator('#'+button).click();
    await page.locator(ready).waitFor(); await idle();
    await visitTabs(value);
  }
  await page.locator('a[data-value="tab_eda"]').click();
  await page.locator('a[role="tab"]').filter({hasText:'Outlier Detection'}).click();
  await page.locator('#eda-remove_outliers').click(); await idle();
  await page.locator('#eda-restore_values').click(); await idle();
  console.log('EXCLUDE/RESTORE OK');
  await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
