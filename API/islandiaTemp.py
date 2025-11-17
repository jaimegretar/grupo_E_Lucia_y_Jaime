import requests
import pandas as pd
from datetime import datetime, date

BASE = "https://api.vedur.is/weather"
CITY_HINT = "Reykjav"              # Cambia por "Akureyri" si quieres esa zona
START = "1980-01-01"               # Puedes ampliar a 2015-01-01 cuando veas cobertura
END   = "2023-12-31"
OUT_PREFIX = "temperatura_reykjavik_2018_2023"
HEADERS = {"Accept": "application/json"}

def normalize_rows(payload):
    if isinstance(payload, list):
        return payload
    if isinstance(payload, dict) and isinstance(payload.get("data"), list):
        return payload["data"]
    if isinstance(payload, dict):
        for v in payload.values():
            if isinstance(v, list):
                return v
    return []

def find_station_id_by_name_hint(name_hint: str) -> int:
    """Busca estación por nombre en /observations/synop/latest"""
    url = f"{BASE}/observations/synop/latest"
    r = requests.get(url, params={"format":"json"}, headers=HEADERS, timeout=60)
    r.raise_for_status()
    rows = normalize_rows(r.json())
    # buscar coincidencia por nombre
    matches = [row for row in rows
               if isinstance(row, dict)
               and isinstance(row.get("name"), str)
               and name_hint.lower() in row["name"].lower()]
    if not matches:
        # Si no hay, intenta devolver la primera de la capital region (region_id=1)
        url2 = f"{BASE}/observations/synop/latest"
        r2 = requests.get(url2, params={"format":"json", "region_id":"1"}, headers=HEADERS, timeout=60)
        r2.raise_for_status()
        rows2 = normalize_rows(r2.json())
        if not rows2:
            raise RuntimeError("No pude listar estaciones (latest).")
        print("⚠ No encontré por nombre; uso primera de la región 1.")
        return int(rows2[0]["station"])
    station_id = int(matches[0]["station"])
    print(f"✔ Estación encontrada: '{matches[0]['name']}' (id={station_id})")
    return station_id

def year_chunks(start: str, end: str):
    sd = datetime.fromisoformat(start).date()
    ed = datetime.fromisoformat(end).date()
    for year in range(sd.year, ed.year + 1):
        a = date(year, 1, 1)
        b = date(year, 12, 31)
        if a < sd: a = sd
        if b > ed: b = ed
        yield a, b

def fetch_synop_agg(aggregation: str, station_id: int, day_from: date, day_to: date) -> pd.DataFrame:
    """
    Llama a /observations/synop/{aggregation} usando station_id como array en query:
    station_id=1&station_id=1  (requests admite lista de tuplas para repetir keys)
    """
    assert aggregation in {"day","month","year","clock"}
    endpoint = f"{BASE}/observations/synop/{aggregation}"
    # lista de tuplas para que 'station_id' salga repetido (array)
    params = [
        ("station_id", station_id),            # array<integer>
        ("day_from", day_from.isoformat()),
        ("day_to",   day_to.isoformat()),
        ("parameters", "basic"),
        ("format", "json"),
        ("order",  "asc"),
    ]
    r = requests.get(endpoint, params=params, headers=HEADERS, timeout=120)
    if r.status_code == 200:
        rows = normalize_rows(r.json())
        return pd.DataFrame(rows)
    # imprime ayuda de depuración
    raise RuntimeError(f"{endpoint} devolvió {r.status_code}: {r.text[:200]}")

def get_daily_t(station_id: int, start: str, end: str) -> pd.DataFrame:
    frames = []
    for a, b in year_chunks(start, end):
        try:
            df = fetch_synop_agg("day", station_id, a, b)
            if df.empty:
                print(f"⚠ Día {a}–{b}: sin filas")
                continue
            # Esperado: columns year, month, day, t (según el esquema)
            required = {"year","month","day","t"}
            if not required.issubset(df.columns):
                raise RuntimeError(f"Estructura inesperada (day). Columnas: {df.columns.tolist()}")
            df["fecha"] = pd.to_datetime(df[["year","month","day"]])
            frames.append(df[["fecha","t"]].rename(columns={"t":"temperatura_media"}).dropna())
            print(f"✔ Día {a}–{b}: {len(df)} filas")
        except Exception as e:
            print(f"❌ Día {a}–{b} error: {e}")
    if not frames:
        return pd.DataFrame(columns=["fecha","temperatura_media"])
    out = (pd.concat(frames, ignore_index=True)
             .sort_values("fecha").reset_index(drop=True))
    return out

def get_monthly_t(station_id: int, start: str, end: str) -> pd.DataFrame:
    frames = []
    for a, b in year_chunks(start, end):
        try:
            df = fetch_synop_agg("month", station_id, a, b)
            if df.empty:
                print(f"⚠ Mes {a}–{b}: sin filas")
                continue
            required = {"year","month","t"}
            if not required.issubset(df.columns):
                raise RuntimeError(f"Estructura inesperada (month). Columnas: {df.columns.tolist()}")
            # para mensual no hay 'day'; ponemos día 1
            df["fecha"] = pd.to_datetime(df[["year","month"]].assign(day=1))
            frames.append(df[["fecha","t"]].rename(columns={"t":"temperatura_media"}).dropna())
            print(f"✔ Mes {a}–{b}: {len(df)} filas")
        except Exception as e:
            print(f"❌ Mes {a}–{b} error: {e}")
    if not frames:
        return pd.DataFrame(columns=["fecha","temperatura_media"])
    out = (pd.concat(frames, ignore_index=True)
             .sort_values("fecha").reset_index(drop=True))
    return out

def save(df: pd.DataFrame, prefix: str):
    df.to_csv(f"{prefix}.csv", index=False, encoding="utf-8")
    df.to_json(f"{prefix}.json", orient="records", indent=2, date_format="iso")
    print("\n✅ Guardado en:")
    print(f" - {prefix}.csv")
    print(f" - {prefix}.json")

if __name__ == "__main__":
    station_id = find_station_id_by_name_hint(CITY_HINT)

    # 1) Intento diario
    daily = get_daily_t(station_id, START, END)

    if not daily.empty:
        print(daily.head())
        save(daily, OUT_PREFIX)                      # salida diaria
    else:
        print("↩️ No hubo datos diarios; paso a semanal (re-muestreo) o mensual.")
        # 2a) Intento semanal a partir de 'clock' o 'day' => si no hay 'day', usa 'month'
        monthly = get_monthly_t(station_id, START, END)
        if monthly.empty:
            raise SystemExit("❌ No hubo datos diarios ni mensuales en el rango. Prueba otra estación o rango.")

        # si necesitas semanal, puedes expandir mensual a semanal por forward-fill (opcional):
        # aquí dejamos mensual porque la API soporta 'month' nativamente
        save(monthly, OUT_PREFIX + "_mensual")
        print("ℹ He guardado la serie mensual. Si quieres, te genero semanal a partir de la mensual.")
