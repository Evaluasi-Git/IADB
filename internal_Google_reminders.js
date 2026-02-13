/**
 * Author: Cedric Antunes
 * Date: February, 2026
 */

const CONFIG = {
  SHEET_NAME: "Payment Schedule",
  HEADER_ROW: 1,
  CALENDAR_ID: "cantunes@evaluasi.io",    // Evaluasi calendar ID
  EVENT_TIME_HOUR: 9,                     // event start time on send_by_date
  EVENT_TIME_MIN: 0,
  EVENT_DURATION_MIN: 20,

  // Reminders (minutes before start)
  POPUP_REMINDER_MIN: 10,
  EMAIL_REMINDER_MIN: 60,

  // Throttle 
  MAX_EVENTS_PER_RUN: 40,
  SLEEP_MS_BETWEEN_CREATES: 300,
  MAX_RETRIES: 6,

  // Required columns in sheet
  COL_SEND_BY_DATE: "send_by_date",

  // Creating this column if missing; stores daily event ids (one per date)
  COL_DAILY_EVENT_ID: "calendar_event_id_daily",

  // Better checklist text
  COL_CONF_NAME: "confederate_name",
  COL_CONF_ID: "confederate_id",
  COL_ORDER: "transaction_order",
  COL_CHANNEL: "channel",
  COL_AMOUNT: "amount_usd",
  COL_DELIVERY: "delivery_method",
  COL_TX_DATE: "approximate_date",
  COL_TX_DATETIME: "transaction_datetime" 
};

function createDailySendFundEvents() {
  const ss = SpreadsheetApp.getActiveSpreadsheet();
  const sh = ss.getSheetByName(CONFIG.SHEET_NAME);
  if (!sh) throw new Error(`Sheet not found: ${CONFIG.SHEET_NAME}`);

  const values = sh.getDataRange().getValues();
  if (values.length <= CONFIG.HEADER_ROW) return;

  const headers = values[CONFIG.HEADER_ROW - 1].map(h => String(h).trim());
  const colIndex = (name) => headers.indexOf(name);

  const iSendBy = colIndex(CONFIG.COL_SEND_BY_DATE);
  if (iSendBy === -1) throw new Error(`Missing column: ${CONFIG.COL_SEND_BY_DATE}`);

  // Ensuring daily event ID column exists
  let iDailyEventId = colIndex(CONFIG.COL_DAILY_EVENT_ID);
  if (iDailyEventId === -1) {
    iDailyEventId = headers.length;
    sh.getRange(CONFIG.HEADER_ROW, iDailyEventId + 1).setValue(CONFIG.COL_DAILY_EVENT_ID);
    // rerun once header exists
    return createDailySendFundEvents();
  }

  // Optional columns (safe if missing)
  const iName     = colIndex(CONFIG.COL_CONF_NAME);
  const iCID      = colIndex(CONFIG.COL_CONF_ID);
  const iOrder    = colIndex(CONFIG.COL_ORDER);
  const iChannel  = colIndex(CONFIG.COL_CHANNEL);
  const iAmount   = colIndex(CONFIG.COL_AMOUNT);
  const iDelivery = colIndex(CONFIG.COL_DELIVERY);
  const iTxDate   = colIndex(CONFIG.COL_TX_DATE);
  const iTxDT     = colIndex(CONFIG.COL_TX_DATETIME);

  const cal = CalendarApp.getCalendarById(CONFIG.CALENDAR_ID);
  if (!cal) throw new Error(`Calendar not found: ${CONFIG.CALENDAR_ID}`);

  // Building: send_by_date (YYYY-MM-DD) -> list of row objects
  const byDate = new Map();

  for (let r = CONFIG.HEADER_ROW; r < values.length; r++) {
    const row = values[r];
    const sendByVal = row[iSendBy];
    if (!sendByVal) continue;

    const d = normalizeToDate(sendByVal);
    if (!d) continue;

    const key = ymd_(d); // YYYY-MM-DD

    if (!byDate.has(key)) byDate.set(key, []);
    byDate.get(key).push({
      rowIndex1: r + 1, // 1-based sheet row number (for linking/debug)
      confName:  (iName !== -1 && row[iName]) ? row[iName] : "",
      confId:    (iCID  !== -1 && row[iCID] !== "" && row[iCID] != null) ? row[iCID] : "",
      order:     (iOrder !== -1 && row[iOrder] !== "" && row[iOrder] != null) ? row[iOrder] : "",
      channel:   (iChannel !== -1 && row[iChannel]) ? row[iChannel] : "",
      amount:    (iAmount !== -1 && row[iAmount] !== "" && row[iAmount] != null) ? row[iAmount] : "",
      delivery:  (iDelivery !== -1 && row[iDelivery]) ? row[iDelivery] : "",
      txDate:    (iTxDate !== -1 && row[iTxDate]) ? row[iTxDate] : "",
      txDT:      (iTxDT !== -1 && row[iTxDT]) ? row[iTxDT] : ""
    });
  }

  if (byDate.size === 0) {
    SpreadsheetApp.getUi().alert("No rows with send_by_date found.");
    return;
  }

  // Reading existing daily event IDs
  // Storing a mapping in PropertiesService so each date maps to one event id
  const props = PropertiesService.getDocumentProperties();
  const createdMap = getDailyEventMap_(props); // { "YYYY-MM-DD": "eventId" }

  // Sorting dates
  const dates = Array.from(byDate.keys()).sort();

  let created = 0;
  let skippedExisting = 0;

  for (const key of dates) {
    if (created >= CONFIG.MAX_EVENTS_PER_RUN) break;

    if (createdMap[key]) {
      skippedExisting++;
      continue;
    }

    const rows = byDate.get(key);

    // Building event start/end on that date
    const sendByDate = new Date(key + "T00:00:00");
    sendByDate.setHours(0, 0, 0, 0);

    const start = new Date(sendByDate);
    start.setHours(CONFIG.EVENT_TIME_HOUR, CONFIG.EVENT_TIME_MIN, 0, 0);
    const end = new Date(start.getTime() + CONFIG.EVENT_DURATION_MIN * 60 * 1000);

    const title = `SEND FUNDS (Daily) — ${key} — ${rows.length} payment(s)`;

    const desc = buildDailyChecklistDescription_(key, rows, ss.getUrl());

    Utilities.sleep(CONFIG.SLEEP_MS_BETWEEN_CREATES);

    const event = createEventWithRetry_(cal, title, start, end, desc);

    if (CONFIG.POPUP_REMINDER_MIN != null) event.addPopupReminder(CONFIG.POPUP_REMINDER_MIN);
    if (CONFIG.EMAIL_REMINDER_MIN != null) event.addEmailReminder(CONFIG.EMAIL_REMINDER_MIN);

    createdMap[key] = event.getId();
    created++;
  }

  // Persist mapping
  props.setProperty("DAILY_EVENT_MAP_JSON", JSON.stringify(createdMap));

  writeDailyIdsToSheet_(sh, values, iSendBy, iDailyEventId, createdMap);

  // Computing remaining
  let remaining = 0;
  for (const key of dates) if (!createdMap[key]) remaining++;

  SpreadsheetApp.getUi().alert(
    `Daily reminders:\n` +
    `Created this run: ${created}\n` +
    `Already existed (skipped): ${skippedExisting}\n` +
    `Remaining dates without an event: ${remaining}\n\n` +
    (remaining > 0 ? "Run createDailySendFundEvents() again to continue." : "All dates covered ✅")
  );
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

function writeDailyIdsToSheet_(sh, values, iSendBy, iDailyEventId, createdMap) {
  const nDataRows = values.length - CONFIG.HEADER_ROW;
  const out = sh.getRange(CONFIG.HEADER_ROW + 1, iDailyEventId + 1, nDataRows, 1).getValues();

  for (let r = CONFIG.HEADER_ROW; r < values.length; r++) {
    const sendByVal = values[r][iSendBy];
    if (!sendByVal) continue;
    const d = normalizeToDate(sendByVal);
    if (!d) continue;
    const key = ymd_(d);
    const id = createdMap[key] || "";
    out[r - CONFIG.HEADER_ROW][0] = id;
  }

  sh.getRange(CONFIG.HEADER_ROW + 1, iDailyEventId + 1, nDataRows, 1).setValues(out);
}

function getDailyEventMap_(props) {
  const raw = props.getProperty("DAILY_EVENT_MAP_JSON");
  if (!raw) return {};
  try {
    const obj = JSON.parse(raw);
    return (obj && typeof obj === "object") ? obj : {};
  } catch (e) {
    return {};
  }
}

function createEventWithRetry_(cal, title, start, end, desc) {
  let lastErr = null;

  for (let attempt = 0; attempt <= CONFIG.MAX_RETRIES; attempt++) {
    try {
      return cal.createEvent(title, start, end, { description: desc });
    } catch (e) {
      lastErr = e;
      const msg = String(e && e.message ? e.message : e);

      // Backoff on throttling
      if (msg.includes("too many calendars") || msg.includes("too many") || msg.includes("Rate Limit")) {
        const sleepMs = Math.min(60000, Math.pow(2, attempt) * 1000);
        Utilities.sleep(sleepMs);
        continue;
      }
      throw e;
    }
  }

  throw lastErr;
}

function normalizeToDate(val) {
  if (Object.prototype.toString.call(val) === "[object Date]" && !isNaN(val)) return val;
  const d = new Date(val);
  if (isNaN(d)) return null;
  return d;
}

function ymd_(d) {
  const yyyy = d.getFullYear();
  const mm = String(d.getMonth() + 1).padStart(2, "0");
  const dd = String(d.getDate()).padStart(2, "0");
  return `${yyyy}-${mm}-${dd}`;
}

function buildDailyChecklistDescription_(ymd, rows, sheetUrl) {
  // Sort by confederate then order if present
  rows.sort((a, b) => {
    const an = String(a.confName || "").toLowerCase();
    const bn = String(b.confName || "").toLowerCase();
    if (an < bn) return -1;
    if (an > bn) return 1;
    return (Number(a.order) || 0) - (Number(b.order) || 0);
  });

  const lines = [];
  lines.push(`DAILY SEND-FUNDS CHECKLIST`);
  lines.push(`Send-by date: ${ymd}`);
  lines.push(`Total payments due: ${rows.length}`);
  lines.push("");
  lines.push("Checklist (one line per transaction):");

  rows.forEach((x, idx) => {
    const who = x.confName ? x.confName : (x.confId ? `Conf ${x.confId}` : "Confederate");
    const ord = x.order !== "" ? `#${x.order}` : "";
    const ch  = x.channel ? x.channel : "";
    const am  = x.amount !== "" ? `$${x.amount}` : "";
    const del = x.delivery ? `(${x.delivery})` : "";
    const txd = x.txDate ? `tx_date=${x.txDate}` : "";
    const rowRef = `row=${x.rowIndex1}`;

    lines.push(`- [ ] ${who} ${ord} — ${ch} ${am} ${del} ${txd} ${rowRef}`.replace(/\s+/g, " ").trim());
  });

  lines.push("");
  lines.push("Open source sheet:");
  lines.push(sheetUrl);

  return lines.join("\n");
}
