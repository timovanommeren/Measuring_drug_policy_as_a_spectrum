/* involvement_browser.js
   ------------------------------------------------------------------------
   Draws the involvement x bimodality browser from window.IV_DATA, which
   sections/involvement.Rmd emits as JSON.

   It lived inline in all three pilot parents until it was needed a fourth
   time. Three byte-identical copies of two hundred lines is a drift waiting
   to happen, so it moved here and the section that produces its data inlines
   it - the same arrangement descriptives_browser.js already had. */

/* ---- involvement bimodality browser ---- */
(function(){
  var D = window.IV_DATA;
  if(!D){ return; }
  // default: combined definition, lowest subgroup, survey order
  var state = { def: D.defs[D.defs.length - 1], sub: 1, order: 'survey' };

  // jsonlite(auto_unbox = TRUE) turns a length-1 vector into a scalar, so
  // normalise the mode positions back into an array before using them.
  function modePositions(st){
    var mp = st.modePos;
    if (mp === null || mp === undefined) return [];
    return Array.isArray(mp) ? mp : [mp];
  }

  // Colour the categories that ReMoDe actually identified as MODES, so the
  // modality estimate can be read straight off the chart:
  //   dark red = a detected mode away from the centre (a "pole")
  //   orange   = a detected mode AT the neutral middle category - this is what
  //              separates "weakly" from "strongly" polarised in the
  //              classification, so it gets its own colour
  //   blue     = ordinary category, not a mode
  function barColour(i, st, belief){
    var mp = modePositions(st);
    if (mp.indexOf(i + 1) === -1) return '#9FB6CE';        // not a mode
    // a 9-rule policy item has its own middle category
    var mid = (belief && belief.middleIndex) || D.middleIndex;
    return (i + 1 === mid) ? '#D9822B' : '#B22222';
  }

  function svgFor(belief){
    var st = belief.states[state.def][state.sub - 1];
    // the policy-slider items carry their own category labels
    var shortLab = belief.scaleShort || D.scaleShort;
    var k = shortLab.length, yMax = belief.yLimit;
    var W = 720, H = 200, mL = 40, mR = 10, mT = 8, mB = 34;
    var pw = W - mL - mR, ph = H - mT - mB;
    var bw = pw / k * 0.7, gap = pw / k;
    var s = '<svg viewBox="0 0 ' + W + ' ' + H + '" style="width:100%;height:auto;">';
    // y grid: 0 and yMax
    s += '<line x1="' + mL + '" y1="' + (mT + ph) + '" x2="' + (W - mR) +
         '" y2="' + (mT + ph) + '" stroke="#ccc"/>';
    s += '<line x1="' + mL + '" y1="' + mT + '" x2="' + (W - mR) +
         '" y2="' + mT + '" stroke="#eee"/>';
    s += '<text x="' + (mL - 5) + '" y="' + (mT + 4) + '" font-size="10" ' +
         'text-anchor="end" fill="#888">' + yMax + '%</text>';
    s += '<text x="' + (mL - 5) + '" y="' + (mT + ph) + '" font-size="10" ' +
         'text-anchor="end" fill="#888">0%</text>';
    for(var i = 0; i < k; i++){
      var p = st.prop[i] || 0;
      var h = Math.min(p, yMax) / yMax * ph;
      var x = mL + gap * i + (gap - bw) / 2;
      var y = mT + ph - h;
      s += '<rect x="' + x + '" y="' + y + '" width="' + bw + '" height="' + h +
           '" fill="' + barColour(i, st, belief) + '"/>';
      if(p > 0){
        s += '<text x="' + (x + bw / 2) + '" y="' + (y - 3) + '" font-size="10" ' +
             'text-anchor="middle" fill="#444">' + Math.round(p) + '%</text>';
      }
      s += '<text x="' + (x + bw / 2) + '" y="' + (mT + ph + 14) + '" font-size="11" ' +
           'text-anchor="middle" fill="#333">' + shortLab[i] + '</text>';
    }
    return s + '</svg>';
  }

  var POLCOL = ['#3B6EA5', '#D9822B', '#B22222'];   // none / weak / strong

  function subtitleFor(belief){
    var st = belief.states[state.def][state.sub - 1];
    var sd = (st.sd === null || st.sd === undefined) ? '–' : st.sd;
    var warn = st.n < D.minNRank ? '  ⚠ small subgroup (excluded from ranking)' : '';
    // stability is only computed for the two subgroups that drive the ranking
    var stab = (st.stableUntil === null || st.stableUntil === undefined) ? '' :
               '  |  stable to ' + st.stableUntil + '% removal';
    return 'n = ' + st.n + '  |  modes (ReMoDe) = ' + st.modes +
           '  |  ' + D.polNames[st.pol] +
           '  |  SD = ' + sd + '  |  middle category = ' + st.pctMid + '%' +
           stab + warn;
  }

  // Ranking follows Hoffstadt et al. (2025): the primary criterion is the change
  // in POLARISATION CLASS (not polarised / weakly / strongly) between the lowest
  // and highest sufficiently large involvement subgroup; ties are broken by how
  // many of their six supporting indicators are satisfied.
  function polDelta(belief){
    var c = belief.crit[state.def];
    if(!c || c.polHigh === null || c.polLow === null) return -99;
    return c.polHigh - c.polLow;
  }
  function critScore(belief){
    var c = belief.crit[state.def];
    return (!c || c.score === null) ? -1 : c.score;
  }

  // re-order the belief blocks in the DOM: survey position, or polarisation
  // increase for the currently selected involvement definition.
  function orderItems(){
    var mount = document.getElementById('iv-mount');
    var items = Array.prototype.slice.call(mount.querySelectorAll('.iv-belief'));
    if(state.order === 'polinc'){
      items.sort(function(a, b){
        var A = D.beliefs[+a.getAttribute('data-idx')],
            B = D.beliefs[+b.getAttribute('data-idx')];
        return (polDelta(B) - polDelta(A)) || (critScore(B) - critScore(A));
      });
    } else {
      items.sort(function(a, b){
        return (+a.getAttribute('data-idx')) - (+b.getAttribute('data-idx'));
      });
    }
    items.forEach(function(el){ mount.appendChild(el); });
  }

  function critLine(belief){
    var c = belief.crit[state.def];
    if(!c || c.score === null){
      return '<span style="color:#999">too few respondents per subgroup to rank</span>';
    }
    var d = c.polHigh - c.polLow;
    var arrow = (d > 0 ? '&#9650; ' : d < 0 ? '&#9660; ' : '');
    var col = d > 0 ? '#B22222' : (d < 0 ? '#3B6EA5' : '#777');
    var met = D.critNames.filter(function(nm, i){ return c.checks[i] === 1; });
    // jackknife stability of the two classifications this comparison rests on
    var flag = '';
    if (D.stability) {
      var fmt = function(v){ return (v === null || v === undefined) ? 'n/a' : v + '%'; };
      flag = c.unstable === 1
        ? ' &nbsp;·&nbsp; <span style="color:#B22222" title="the mode count for ' +
          'at least one end does not survive ' + D.stableMin + '% of the data ' +
          'being removed">&#9888; unstable (' + fmt(c.stabLow) + ' / ' +
          fmt(c.stabHigh) + ')</span>'
        : ' &nbsp;·&nbsp; <span style="color:#2e7d32">&#10003; stable (' +
          fmt(c.stabLow) + ' / ' + fmt(c.stabHigh) + ')</span>';
    }
    return '<span style="color:' + col + '"><b>' + arrow +
           D.polNames[c.polLow] + ' &rarr; ' + D.polNames[c.polHigh] +
           '</b></span> (subgroup ' + c.loGrp + ' vs ' + c.hiGrp + ')' +
           ' &nbsp;·&nbsp; supporting indicators ' + c.score + '/6' +
           (met.length ? ': ' + met.join(', ') : '') + flag;
  }

  function redraw(){
    D.beliefs.forEach(function(belief, idx){
      document.getElementById('iv-sub-' + idx).textContent = subtitleFor(belief);
      document.getElementById('iv-crit-' + idx).innerHTML = critLine(belief);
      document.getElementById('iv-svg-' + idx).innerHTML = svgFor(belief);
    });
    // readout + slider
    var rng = D.binRanges[state.def] ? D.binRanges[state.def][state.sub - 1] : '';
    document.getElementById('iv-readout').textContent =
      'Involvement ' + D.binLabels[state.sub - 1] +
      (rng ? ' (score ' + rng + ')' : '') +
      '  —  subgroup ' + state.sub + ' of ' + D.binLabels.length;
    document.getElementById('iv-slider').value = state.sub;
    // active definition button
    Array.prototype.forEach.call(
      document.querySelectorAll('.iv-def-btn'), function(b){
        b.classList.toggle('active', b.getAttribute('data-def') === state.def);
      });
  }

  window.ivSetSub = function(v){
    state.sub = Math.max(1, Math.min(D.binLabels.length, v)); redraw();
  };
  window.ivStep   = function(d){ ivSetSub(state.sub + d); };
  function setDef(dn){ state.def = dn; orderItems(); redraw(); }
  window.ivToggleOrder = function(btn){
    state.order = (state.order === 'survey') ? 'polinc' : 'survey';
    btn.setAttribute('data-current', state.order);
    btn.textContent = (state.order === 'polinc')
      ? 'Sorted by increase in polarisation (low→high involvement) — click for survey order'
      : 'Survey order — click to sort by increase in polarisation';
    orderItems();
  };

  function build(){
    // the number of subgroups depends on how they were formed (4 quartiles with
    // the authors' split, 5 otherwise), so size the slider from the data
    var sl = document.getElementById('iv-slider');
    sl.max = D.binLabels.length;
    // definition buttons
    var db = document.getElementById('iv-def-buttons');
    D.defs.forEach(function(dn){
      var b = document.createElement('button');
      b.className = 'iv-def-btn'; b.setAttribute('data-def', dn);
      b.textContent = D.defLabels[dn];
      b.onclick = function(){ setDef(dn); };
      db.appendChild(b);
    });
    // legend mapping the short codes to full scale labels
    document.getElementById('iv-legend').innerHTML =
      'Scale: ' + D.scaleShort.map(function(c, i){
        return '<b>' + c + '</b> ' + D.scaleLabels[i];
      }).join(' &nbsp;·&nbsp; ') +
      '<br><span style="color:#B22222">&#9632;</span> mode detected by ReMoDe ' +
      '(a pole) &nbsp;·&nbsp; <span style="color:#D9822B">&#9632;</span> mode at ' +
      'the neutral middle category (keeps the item only <i>weakly</i> polarised) ' +
      '&nbsp;·&nbsp; <span style="color:#9FB6CE">&#9632;</span> not a mode.';
    // one block per belief
    var mount = document.getElementById('iv-mount');
    mount.innerHTML = '';                       // clear the loading placeholder
    D.beliefs.forEach(function(belief, idx){
      var wrap = document.createElement('div'); wrap.className = 'iv-belief';
      wrap.setAttribute('data-idx', idx);
      wrap.innerHTML =
        '<div class="iv-title">' + belief.title + '</div>' +
        '<div class="iv-sub" id="iv-sub-' + idx + '"></div>' +
        '<div class="iv-sub" id="iv-crit-' + idx + '"></div>' +
        '<div id="iv-svg-' + idx + '"></div>';
      mount.appendChild(wrap);
    });
    orderItems();
    redraw();
  }

  // run now if the DOM is already parsed, otherwise wait for it -- robust to
  // scripts that execute after DOMContentLoaded has already fired.
  if (document.readyState !== 'loading') { build(); }
  else { document.addEventListener('DOMContentLoaded', build); }
})();
