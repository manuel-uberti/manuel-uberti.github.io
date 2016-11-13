var U = {
  $: function(id) {
    'use strict';
    if (typeof id == 'string') {
      return document.getElementById(id);
    }
  }};

function calculate1() {
  'use strict';

  var totalRows;
  var orows = U.$('orows').value;
  var rows = U.$('rows').value;
  var ovalr = U.$('ovalr').value;
  U.$('message1').innerHTML = "";

  if ( (orows == null || orows == "") ||
       (rows == null || rows == "") ||
       (ovalr == null || ovalr == "") ) {
    U.$('message1').innerHTML = "Please complete all the fields!";
    return false;
  }

  totalRows = (rows * ovalr) / orows;
  totalRows = totalRows.toFixed(2);

  U.$('totalr').innerHTML = "<strong>" + totalRows + "</strong>";

  return false;
} // end of calculate1()

function calculate1b() {
  'use strict';

  var totalSts;
  var osts = U.$('osts').value;
  var sts = U.$('sts').value;
  var ovals = U.$('ovals').value;
  U.$('message1b').innerHTML = "";

  if ( (osts == null || osts == "") ||
       (sts == null || sts == "") ||
       (ovals == null || ovals == "") ) {
    U.$('message1b').innerHTML = "Please complete all the fields!";
    return false;
  }

  totalSts = (sts * ovals) / osts;
  totalSts = totalSts.toFixed(2);

  U.$('totals').innerHTML = "<strong>" + totalSts + "</strong>";

  return false;
} // end of calculate1b()

function calculate2() {
  'use strict';

  var finalSts;
  var units = U.$('units').value;
  var sts = U.$('sts1').value;
  var cm = U.$('cm').value;
  U.$('message2').innerHTML = "";

  if ( (units == null || units == "") ||
       (sts == null || sts == "") ||
       (cm == null || cm == "") ) {
    U.$('message2').innerHTML = "Please complete all the fields!";
    return false;
  }

  finalSts = (sts / units) * cm;
  finalSts = finalSts.toFixed(2);

  U.$('finals').innerHTML = "<strong>" + finalSts + "</strong>";

  return false;
} // end of calculate2()

function calculate3() {
  'use strict';

  var finaly;
  var weightf = U.$('weightf').value;
  var yardf = U.$('yardf').value;
  var weightp = U.$('weightp').value;
  U.$('message3').innerHTML = "";

  if ( (weightf == null || weightf == "") ||
       (yardf == null || yardf == "") ||
       (weightp == null || weightp == "") ) {
    U.$('message3').innerHTML = "Please complete all the fields!";
    return false;
  }

  finaly = (yardf * weightp) / weightf;
  finaly = finaly.toFixed(2);

  U.$('finaly').innerHTML = "<strong>" + finaly + "</strong>";

  return false;
} // end of calculate3()

function calculate4() {
  'use strict';

  var totalshb;
  var numbershb = U.$('numbershb').value;
  var yardageshb = U.$('yardageshb').value;
  var yardagesub = U.$('yardagesub').value;
  U.$('message4').innerHTML = "";

  if ( (numbershb == null || numbershb == "") ||
       (yardageshb == null || yardageshb == "") ||
       (yardagesub == null || yardagesub == "") ) {
    U.$('message4').innerHTML = "Please complete all the fields!";
    return false;
  }

  totalshb = (numbershb * yardageshb) / yardagesub;
  totalshb = totalshb.toFixed(2);

  U.$('totalshb').innerHTML = "<strong>" + totalshb + "</strong>";

  return false;
} // end of calculate4()

function clean1() {
  'use strict';

  U.$('orows').value = "";
  U.$('rows').value = "";
  U.$('ovalr').value = "";
  U.$('totalr').innerHTML = "";
  U.$('message1').innerHTML = "";

  return false;
} // end of clean1()

function clean1b() {
  'use strict';

  U.$('osts').value = "";
  U.$('sts').value = "";
  U.$('ovals').value = "";
  U.$('totals').innerHTML = "";
  U.$('message1b').innerHTML = "";

  return false;
} // end of clean1b()

function clean2() {
  'use strict';

  U.$('units').value = "";
  U.$('sts1').value = "";
  U.$('cm').value = "";
  U.$('finals').innerHTML = "";
  U.$('message2').innerHTML = "";

  return false;
} // end of clean2()

function clean3() {
  'use strict';

  U.$('weightf').value = "";
  U.$('yardf').value = "";
  U.$('weightp').value = "";
  U.$('finaly').innerHTML = "";
  U.$('message3').innerHTML = "";

  return false;
} // end of clean3()

function clean4() {
  'use strict';

  U.$('numbershb').value = "";
  U.$('yardageshb').value = "";
  U.$('yardagesub').value = "";
  U.$('totalshb').innerHTML = "";
  U.$('message4').innerHTML = "";

  return false;
} // end of clean4()

function init () {
  'use strict';
  U.$('button1').onclick = calculate1;
  U.$('clean1').onclick = clean1;
  U.$('button1b').onclick = calculate1b;
  U.$('clean1b').onclick = clean1b;
  U.$('button2').onclick = calculate2;
  U.$('clean2').onclick = clean2;
  U.$('button3').onclick = calculate3;
  U.$('clean3').onclick = clean3;
  U.$('button4').onclick = calculate4;
  U.$('clean4').onclick = clean4;
} // end of init()

window.onload = init;
