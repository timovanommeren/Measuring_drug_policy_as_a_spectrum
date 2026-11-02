/* ===========================================================================
   descriptives_browser.js
   ---------------------------------------------------------------------------
   Draws every descriptive chart on the first tab as inline SVG, from the JSON
   cube produced by R/descriptives_cube.R (window.DESC_DATA).

   Why client-side: the charts can be split by demographic group at the click of
   a button. Pre-rendering every question x every split would mean hundreds of
   images in the file; drawing them here keeps the HTML small and the switching
   instant.

   Display per split:
     all       one bar per response category (counts)
     grouped   one bar per group, side by side, as PERCENTAGE WITHIN GROUP -
               groups differ in size (links 59 vs rechts 17), so raw counts
               would compare group size rather than response pattern
     stacked   one bar per category, split into the groups (counts), which shows
               the response distribution and its composition at once - used for
               the splits with many ordered levels (age bands, education)

   One exception: blocks flagged `multi` in the data - the emotion charts - are
   MULTI-SELECT, so a respondent can appear in several bars and the bars do not
   sum to the group. Those stay in counts under every split, because a
   percentage there reads as a share of the group and is no such thing.

   Slider blocks additionally carry a smoothed density of the underlying
   continuous 0-100 values, drawn BEHIND the bars at low opacity.
   =========================================================================== */
(function () {
  var D = window.DESC_DATA;
  if (!D) return;

  // `counts` overrides the axis for the ordinary blocks. The default is
  // percentages, because groups differ a lot in size and raw counts would
  // compare group size rather than response pattern; the button is there for
  // when the underlying numbers matter more than comparability. The emotion
  // charts ignore it and stay in counts either way - see `block.multi` below.
  var state = { split: 'all', order: {}, counts: false };

  var PALETTE = {
    politics: ['#3B6EA5', '#8E7CC3', '#B22222'],           // links / midden / rechts
    gender:   ['#3B6EA5', '#D9822B'],                       // Man / Vrouw
    age:      ['#DCE6F1', '#B8CCE4', '#95AFD1', '#7292BE',  // light -> dark
               '#4F76AB', '#3B6EA5', '#27496B'],
    // Substance use is an ORDERED variable, so it gets an ordered ramp running
    // green -> amber -> red as involvement increases. Deliberately unlike the
    // blue-based politics and age palettes, so the reader never confuses which
    // split is on screen.
    substance: ['#2E7D5B', '#7FB069', '#E8A33D', '#C1462F'],
    // The history split re-cuts only its first two groups; its last two are the
    // very same respondents as the substance split. The palette says so: the
    // amber and red are repeated exactly, and only the first two colours differ.
    history:   ['#40004B', '#9970AB', '#E8A33D', '#C1462F'],

    // ---- emotion splits -----------------------------------------------------
    // Every emotion topic uses the SAME six colours in the same order, so a hue
    // always means one emotion whichever topic is on screen and the topics can
    // be compared by eye:
    //     magenta #C51B7D  boos
    //     purple  #6A3D9A  bang / bezorgd
    //     l.teal  #7FCDBB  hoop
    //     teal    #1D8F8A  blij
    //     grey    #9E9E9E  geen van deze
    // Deliberately unlike the blue demographic palettes and the green-to-red
    // substance ones.
    emotion:   ['#C51B7D', '#6A3D9A', '#7FCDBB', '#1D8F8A', '#9E9E9E']
  };
  // every emo_<topic> split shares the one emotion palette
  ['alcohol', 'tabak', 'softdrugs', 'harddrugs', 'drugsbeleid'].forEach(function (t) {
    PALETTE['emo_' + t] = PALETTE.emotion;
  });
  // Drugsaffect summarises the two drug topics into one ORDERED scale, and its
  // groups run from most negative to most positive on attitude as well
  // (3.96 / 4.43 / 4.94 / 5.13). It therefore gets a red-to-green ramp rather
  // than the categorical emotion hues: red = negative feeling, green = positive.
  // Note this runs the opposite way to the substance palettes, which start
  // green at low use - the family colour on the button says which is which.
  PALETTE.affect = ['#C1462F', '#E8A33D', '#9CCB86', '#2E7D5B'];

  // ---- second-pilot splits ---------------------------------------------------
  // Both are yes/no questions rather than ordered scales, so they get two
  // clearly contrasting hues rather than a ramp.
  PALETTE.stad = ['#3B6EA5', '#8FA9C4'];       // grote stad / geen grote stad
  PALETTE.mdma = ['#B22222', '#2E7D5B'];       // tegen / voor regulering
  // Education is ORDERED and stacked, like the age bands, so it gets the same
  // light-to-dark ramp: six steps for the survey's six answer options.
  PALETTE.opleiding = ['#DCE6F1', '#B8CCE4', '#95AFD1', '#7292BE',
                       '#4F76AB', '#27496B'];

  var PLAIN = '#3B6EA5';

  // A split with no palette of its own used to take the whole tab down: the
  // lookup returned undefined and reading `.length` off it threw, so from the
  // first click on that button every chart, the legend and the other control
  // buttons stopped responding. `stad` and `mdma` were added as splits without
  // a palette and did exactly that. A split is data; a missing colour must
  // never be fatal, so it falls back to the demographic blues.
  var FALLBACK = ['#3B6EA5', '#D9822B', '#8E7CC3', '#2E7D5B', '#B22222',
                  '#E8A33D', '#7292BE'];
  function paletteFor(split) { return PALETTE[split] || FALLBACK; }

  // Axis ticks at round intervals (1 / 2 / 5 x 10^n), so the labels are numbers
  // a reader recognises and the top of the axis is a sensible ceiling rather
  // than an arbitrary padded value.
  function niceAxis(dataMax, targetTicks) {
    var target = targetTicks || 5;
    var raw = Math.max(dataMax, 1) / target;
    var mag = Math.pow(10, Math.floor(Math.log(raw) / Math.LN10));
    var norm = raw / mag;
    var step = (norm <= 1 ? 1 : norm <= 2 ? 2 : norm <= 5 ? 5 : 10) * mag;
    var top = Math.ceil(Math.max(dataMax, 1) / step) * step;
    var ticks = [];
    for (var v = 0; v <= top + step * 1e-6; v += step) ticks.push(v);
    return { ticks: ticks, top: top, step: step };
  }

  function series(item, block) {
    // returns [{label, colour, values[], n}] for the current split
    if (state.split === 'all') {
      return [{ label: 'alle respondenten', colour: PLAIN,
                values: item.counts.all, n: D.N, pct: false }];
    }
    var dem = D.demographics[state.split];
    var pal = paletteFor(state.split);
    return item.counts[state.split].map(function (v, i) {
      return { label: dem.groups[i], colour: pal[i % pal.length],
               // Counts rather than percentages in two situations:
               //   dem.overlapping  the SPLIT is a raw emotion question, whose
               //                    groups share respondents - so the groups do
               //                    not partition the sample and a percentage
               //                    within one of them is not a share of
               //                    anything the reader can add up. Every block
               //                    goes to counts under such a split.
               //   block.multi      the CHART is the emotion question itself,
               //                    where one respondent can fill several bars.
               values: v, n: dem.n[i],
               pct: dem.mode === 'grouped' && !dem.overlapping &&
                    !block.multi && !state.counts };
    });
  }

  function svgFor(item, block) {
    var cats = block.categories, k = cats.length;
    var ss = series(item, block);
    var stacked = state.split !== 'all' &&
                  D.demographics[state.split].mode === 'stacked';

    // convert to the quantity actually plotted
    var vals = ss.map(function (s) {
      return s.values.map(function (v) {
        return s.pct ? (s.n ? 100 * v / s.n : 0) : v;
      });
    });

    var yMax;
    if (stacked) {
      yMax = Math.max.apply(null, cats.map(function (_, c) {
        return vals.reduce(function (a, v) { return a + v[c]; }, 0);
      }));
    } else {
      yMax = Math.max.apply(null, vals.map(function (v) {
        return Math.max.apply(null, v);
      }));
    }
    var axis = niceAxis(yMax);
    yMax = axis.top;

    // ---- geometry -----------------------------------------------------------
    // The category labels are drawn at -45 degrees, so a label of n characters
    // needs about n * charWidth * sin(45) of VERTICAL room. The bottom margin is
    // therefore sized to the longest label in this particular chart rather than
    // fixed: with a fixed margin the long policy names ("verkoop met winst,
    // zonder marketing", 35 characters) ran off the bottom of the viewBox and
    // were simply cut off. The plot area itself keeps a constant height, so the
    // bars stay comparable between charts; only the total height changes.
    var LBL_FS = 10;                       // label font size
    var CHAR_W = LBL_FS * 0.55;            // mean glyph width at that size
    var PER_CHAR = CHAR_W * Math.SQRT1_2;  // vertical room per character at 45 deg
    var LBL_MAX_B = 170;                   // ceiling on the bottom margin
    var maxChars = Math.floor((LBL_MAX_B - 18) / PER_CHAR);
    // Anything past the ceiling is capped with an ellipsis, so a label is either
    // shown in full or visibly shortened - never silently clipped.
    var catsFit = cats.map(function (c) {
      c = String(c);
      return c.length > maxChars ? c.slice(0, maxChars - 1) + '…' : c;
    });
    var longest = catsFit.reduce(function (m, c) { return Math.max(m, c.length); }, 0);
    var W = 720, mL = 46, mR = 8, mT = 10;
    var mB = Math.min(LBL_MAX_B, Math.max(40, Math.ceil(12 + longest * PER_CHAR + 8)));
    var ph = 162;                          // constant plot height
    var H = mT + ph + mB;
    var pw = W - mL - mR;
    var slot = pw / k;
    var y = function (v) { return mT + ph - (v / yMax) * ph; };
    var unit = ss[0].pct ? '%' : '';

    var s = '<svg viewBox="0 0 ' + W + ' ' + H + '" style="width:100%;height:auto;">';

    // ---- gridlines first, so everything else is drawn on top ----------------
    axis.ticks.forEach(function (t) {
      s += '<line x1="' + mL + '" y1="' + y(t).toFixed(1) + '" x2="' + (W - mR) +
           '" y2="' + y(t).toFixed(1) + '" stroke="' +
           (t === 0 ? '#bbb' : '#eee') + '"/>';
      s += '<text x="' + (mL - 6) + '" y="' + (y(t) + 3.5).toFixed(1) +
           '" font-size="9" text-anchor="end" fill="#888">' +
           (Math.round(t * 10) / 10) + unit + '</text>';
    });

    // ---- density curve, drawn first so the bars sit on top ------------------
    if (block.hasDensity && item.density && !stacked) {
      var nAnchor = block.anchorPos.length;
      ss.forEach(function (sr, si) {
        var bySplit = state.split === 'all' ? null : item.density[state.split];
        var dd = state.split === 'all' ? item.density.all
                                       : (bySplit ? bySplit[si] : null);
        // A group too small for a density gets empty coordinates rather than a
        // curve; check the coordinates themselves, since an empty object from
        // the JSON would pass a plain truthiness test.
        if (!dd || !dd.x || !dd.x.length) return;
        var scale = sr.pct ? (sr.n ? 100 / sr.n : 0) : 1;
        var pts = dd.x.map(function (xv, i) {
          // map the 0-100 slider range onto the centres of the anchor slots
          var px = mL + slot / 2 + (xv / 100) * slot * (nAnchor - 1);
          return px.toFixed(1) + ',' + y(dd.y[i] * scale).toFixed(1);
        });
        s += '<polygon points="' + mL + ',' + (mT + ph) + ' ' + pts.join(' ') +
             ' ' + (mL + slot / 2 + slot * (nAnchor - 1)) + ',' + (mT + ph) +
             '" fill="' + sr.colour + '" opacity="0.16"/>';
        s += '<polyline points="' + pts.join(' ') + '" fill="none" stroke="' +
             sr.colour + '" stroke-width="1.6" opacity="0.55"/>';
      });
    }

    // ---- bars ---------------------------------------------------------------
    if (stacked) {
      for (var c = 0; c < k; c++) {
        var base = 0;
        for (var si = 0; si < vals.length; si++) {
          var v = vals[si][c]; if (!v) continue;
          var bw = slot * 0.72, x0 = mL + slot * c + (slot - bw) / 2;
          s += '<rect x="' + x0 + '" y="' + y(base + v) + '" width="' + bw +
               '" height="' + (y(base) - y(base + v)) + '" fill="' +
               ss[si].colour + '"/>';
          base += v;
        }
      }
    } else {
      var g = vals.length, bw2 = (slot * 0.78) / g;
      for (var c2 = 0; c2 < k; c2++) {
        for (var si2 = 0; si2 < g; si2++) {
          var v2 = vals[si2][c2];
          var x1 = mL + slot * c2 + slot * 0.11 + bw2 * si2;
          if (v2 > 0) {
            s += '<rect x="' + x1 + '" y="' + y(v2) + '" width="' + (bw2 * 0.92) +
                 '" height="' + (mT + ph - y(v2)) + '" fill="' + ss[si2].colour + '"/>';
          }
        }
      }
    }

    // ---- category labels ----------------------------------------------------
    // Angled at -45 degrees; the bottom margin above was sized so that every one
    // of these fits inside the viewBox. `title` keeps the untruncated text
    // available on hover for the rare label that had to be capped.
    for (var c3 = 0; c3 < k; c3++) {
      var cx = mL + slot * c3 + slot / 2;
      s += '<text x="' + cx + '" y="' + (mT + ph + 12) +
           '" font-size="' + LBL_FS + '" fill="#333" text-anchor="end" ' +
           'transform="rotate(-45 ' + cx + ',' + (mT + ph + 12) + ')">' +
           (catsFit[c3] === String(cats[c3]) ? '' : '<title>' + esc(cats[c3]) + '</title>') +
           esc(catsFit[c3]) + '</text>';
    }
    return s + '</svg>';
  }

  function esc(t) {
    return String(t).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }

  function legendHtml() {
    if (state.split === 'all') return '';
    var dem = D.demographics[state.split], pal = paletteFor(state.split);
    var parts = dem.groups.map(function (g, i) {
      return '<span style="color:' + pal[i % pal.length] +
             '">&#9632;</span> ' + esc(g) + ' (n = ' + dem.n[i] + ')';
    });
    var note = dem.excluded > 0
      ? ' &nbsp;&middot;&nbsp; <i>' + dem.excluded +
        ' respondent(en) buiten deze groepen, niet getoond</i>' : '';
    var unit = dem.mode === 'grouped'
      ? ' &nbsp;&middot;&nbsp; bars show ' +
        (dem.overlapping
           ? '<i>counts</i> - these groups overlap, so a respondent who ticked ' +
             'two emotions appears in both'
           : (state.counts || null)
             ? '<i>counts</i>'
             : '% <i>within</i> each group (the emotion charts themselves show counts)')
      : ' &nbsp;&middot;&nbsp; gestapelde aantallen';
    return parts.join(' &nbsp; ') + unit + note;
  }

  // Re-order the items of one block by SD or mean (only blocks that carry those
  // statistics, i.e. the ordered-Likert ones). Cycles survey -> sd -> mean.
  window.descToggleOrder = function (blockId, btn) {
    var next = { survey: 'sd', sd: 'mean', mean: 'survey' }[state.order[blockId] || 'survey'];
    state.order[blockId] = next;
    btn.textContent = { survey: 'Volgorde: vragenlijst',
                        sd:     'Volgorde: SD (meest verdeeld eerst)',
                        mean:   'Volgorde: gemiddelde (hoogst eerst)' }[next];
    var mount = document.getElementById('desc-block-' + blockId);
    var kids = Array.prototype.slice.call(mount.querySelectorAll('.iv-belief'));
    kids.sort(function (a, b) {
      var ia = +a.getAttribute('data-idx'), ib = +b.getAttribute('data-idx');
      if (next === 'survey') return ia - ib;
      var blk = D.blocks.filter(function (x) { return x.id === blockId; })[0];
      return blk.items[ib][next] - blk.items[ia][next];
    });
    kids.forEach(function (el) { mount.appendChild(el); });
  };

  // Swap the ordinary grouped charts between "% within group" and raw counts.
  // The emotion charts are unaffected: they are multi-select, so they stay in
  // counts under either setting.
  window.descToggleCounts = function (btn) {
    state.counts = !state.counts;
    btn.textContent = state.counts ? 'as: aantallen' : 'as: % binnen groep';
    btn.classList.toggle('active', state.counts);
    draw();
  };


  window.descSetSplit = function (name, btn) {
    state.split = name;
    Array.prototype.forEach.call(
      document.querySelectorAll('.desc-split-btn'), function (b) {
        b.classList.toggle('active', b === btn);
      });
    draw();
  };

  function draw() {
    document.getElementById('desc-legend').innerHTML = legendHtml();
    D.blocks.forEach(function (block, bi) {
      block.items.forEach(function (item, ii) {
        var el = document.getElementById('desc-svg-' + bi + '-' + ii);
        if (el) el.innerHTML = svgFor(item, block);
      });
    });
  }

  function build() {
    D.blocks.forEach(function (block, bi) {
      var mount = document.getElementById('desc-block-' + block.id);
      if (!mount) return;
      mount.innerHTML = block.items.map(function (item, ii) {
        var sub = (item.mean === undefined) ? '' :
          '<div class="iv-sub">gemiddelde = ' + item.mean + ' &nbsp;|&nbsp; SD = ' +
          item.sd + '</div>';
        return '<div class="iv-belief" data-idx="' + ii + '">' +
               '<div class="iv-title">' + esc(item.title) + '</div>' + sub +
               '<div id="desc-svg-' + bi + '-' + ii + '"></div></div>';
      }).join('');
    });
    draw();
  }

  if (document.readyState !== 'loading') build();
  else document.addEventListener('DOMContentLoaded', build);
})();
