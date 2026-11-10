const {chromium} = require('playwright');
const fs = require('fs');
const path = require('path');
const traits = ['VegP','DTM','RepP','lodging','YLD','YLD_kgha','PRT','CPY_kgha','DS'];
const dataset = process.argv[2];
const label = process.argv[3] || 'current';
const out = path.resolve('validation-output/activate');
fs.mkdirSync(out,{recursive:true});
(async()=>{
  const browser = await chromium.launch({channel:'msedge',headless:true});
  const page = await browser.newPage({viewport:{width:1600,height:1100},acceptDownloads:true});
  page.setDefaultTimeout(90000);
  const logs=[];
  const idle=async()=>{
    await page.waitForTimeout(400);
    await page.waitForFunction(()=>window.Shiny && !document.documentElement.classList.contains('shiny-busy') && !Array.from(document.querySelectorAll('.recalculating')).some(x=>x.getClientRects().length));
  };
  const select=async(id,value)=>{
    await page.locator('#'+id).evaluate((el,v)=>el.selectize ? el.selectize.setValue(v) : window.jQuery(el).val(v).trigger('change'),value); await idle();
    if(label !== 'before' && await page.locator('#'+id).inputValue() !== value) throw Error('Trait not selected: '+value);
  };
  const inspect=async(stage,id)=>{
    const state=await page.evaluate(id=>{
      const el=document.querySelector('#'+id+' .js-plotly-plot') || document.querySelector('#'+id);
      return {errors:Array.from(document.querySelectorAll('.shiny-output-error,.shiny-notification-error')).filter(e=>e.getClientRects().length).map(e=>e.textContent),
        text:el?.textContent, traces:el?.data?.map(t=>({type:t.type,name:t.name,text:t.text,hovertext:t.hovertext,hovertemplate:t.hovertemplate,texttemplate:t.texttemplate,x:t.x,y:t.y}))};
    },id); logs.push({stage,...state}); console.log(stage,JSON.stringify({errors:state.errors,traces:state.traces?.length,text:state.text?.slice(0,180)}));
    fs.writeFileSync(path.join(out,label+'-browser.json'),JSON.stringify(logs,null,2));
    if(label !== 'before' && (state.errors.length || !state.traces?.length || /error:/i.test(state.text||'') ||
      (stage.includes('heatmap') && !state.traces.some(t=>t.type==='heatmap')))) throw Error('Invalid plot: '+stage);
  };
  await page.goto('http://127.0.0.1:3877'); await page.locator('#btn_upload_own').click();
  await page.locator('#data-file_upload').setInputFiles(dataset);
  await page.waitForFunction(()=>document.querySelector('#data-col_gen')?.selectize?.options?.Lentil);
  await page.evaluate(traits=>{
    for(const [id,v] of Object.entries({col_gen:'Lentil',col_env:'ENV',col_rep:'Rep_gen',col_block:'',col_row:'Row',col_col:'Col',col_traits:traits})) {
      const el=document.querySelector('#data-'+id); if(el.selectize) el.selectize.setValue(v); else window.jQuery(el).val(v).trigger('change');
    }
  },traits); await idle();
  console.log('MAPPED',await page.locator('#data-col_traits').inputValue());
  await page.locator('a[data-value="tab_eda"]').click(); await idle();
  await page.locator('div.tab-pane[data-value="tab_eda"] a[role="tab"]').filter({hasText:'G×E Heatmap'}).click(); await idle();
  for(const trait of ['final','visual'].includes(label) ? ['DS'] : traits){
    await select('eda-heat_trait',trait); await inspect('heatmap '+trait,'eda-heatmap');
  }
  await page.screenshot({path:path.join(out,label+'-heatmap.png'),fullPage:true});
  await page.locator('#eda-send_heatmap_report').click(); await idle();
  await page.locator('a[data-value="tab_adaptation"]').click();
  for (const trait of label === 'before' ? ['VegP'] : label === 'visual' ? ['DS'] : traits) {
    await select('adaptation-trait',trait);
    await page.locator('#adaptation-run_adaptation').click(); await idle();
    await inspect('observed winners '+trait,'adaptation-plot_mega_env');
    await page.locator('#adaptation-plot_mega_env').evaluate(el=>window.Plotly.Fx.hover(el,[{curveNumber:0,pointNumber:0}]));
    await page.waitForTimeout(250);
    const hover = (await page.locator('.hoverlayer').allTextContents()).join(' ');
    logs.push({stage:'winner hover '+trait,text:hover});
    if(label !== 'before' && (hover.includes('%{') || !hover.includes('Winner:'))) throw Error('Invalid hover '+hover);
  }
  await page.screenshot({path:path.join(out,label+'-winners.png'),fullPage:true});
  if(label !== 'before') {
    await page.locator('#adaptation-send_mega_env_report').click();
    await page.locator('#adaptation-send_adaptation_tables').click(); await idle();
    await page.locator('a[data-value="tab_stability"]').click();
    await page.locator('div.tab-pane[data-value="tab_stability"] a[role="tab"]').filter({hasText:'GxE Heatmap'}).click();
    for (const trait of label === 'visual' ? ['DS'] : traits) {
      await select('stability-trait',trait);
      await page.locator('#stability-run_stability').click(); await idle();
      await inspect('interaction heatmap '+trait,'stability-plot_heatmap');
    }
    await page.screenshot({path:path.join(out,label+'-interaction.png'),fullPage:true});
    await page.locator('#stability-send_gxe_heatmap_report').click(); await idle();
    await page.locator('a[data-value="tab_reports"]').click(); await idle();
    const download=async(id,name)=>{
      const pending=page.waitForEvent('download'); await page.locator('#'+id).click();
      const file=await pending; await file.saveAs(path.join(out,name));
      if(fs.statSync(path.join(out,name)).size < 100) throw Error('Empty download '+name);
      console.log('DOWNLOAD',name);
    };
    await download('reports-dl_table_csv','adaptation.csv');
    await download('reports-dl_table_xlsx','adaptation.xlsx');
    await page.getByRole('tab',{name:'Registered Plots',exact:true}).click(); await idle();
    const plots=await page.locator('#reports-plot_select').evaluate(el=>Object.values(el.selectize.options).map(o=>({value:o.value,text:o.label||o.text})));
    fs.writeFileSync(path.join(out,'registered-plots.json'),JSON.stringify(plots,null,2));
    for (const [i,plot] of plots.entries()) {
      await select('reports-plot_select',plot.value);
      for(const fmt of ['PNG','SVG','PDF']) {
        await select('reports-plt_format',fmt); await download('reports-dl_plot_file','plot-'+(i+1)+'.'+fmt.toLowerCase());
      }
    }
  }
  fs.writeFileSync(path.join(out,label+'-browser.json'),JSON.stringify(logs,null,2));
  await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
