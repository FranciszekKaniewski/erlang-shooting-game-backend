# 🎮 Shooting Game Server (Erlang/OTP)

**Autor:** Franciszek Kaniewski (UMK, 2026)  
**Typ projektu:** Backend gry multiplayer czasu rzeczywistego  
**Frontend Repository:** [github.com/FranciszekKaniewski/programowanie-funkcyjne-fe](https://github.com/FranciszekKaniewski/programowanie-funkcyjne-fe)

---

## 📋 Opis Projektu

Projekt to serwer gry wieloosobowej napisany w języku **Erlang** z wykorzystaniem wzorców **OTP**. Serwer zarządza stanem gry, fizyką (kolizje), logiką poruszania się oraz synchronizacją graczy w czasie rzeczywistym.

Komunikacja z klientem odbywa się poprzez **WebSockets** (biblioteka Cowboy), a dane przesyłane są w formacie **JSON**.

## 🛠️ Technologie

* **Język:** Erlang/OTP 24+
* **Build Tool:** Rebar3
* **Web Server:** Cowboy 2.12 (obsługa WebSockets)
* **JSON Parser:** JSX 3.1

## 🏗️ Architektura Systemu

System oparty jest na drzewie nadzoru (supervision tree), co gwarantuje stabilność (ang. *fault tolerance*).

### Główne moduły:

1.  **`game_app.erl`**
    * Punkt wejścia aplikacji.
    * Uruchamia serwer HTTP (port 8080) i główny nadzorca.
2.  **`game_sup.erl`**
    * Główny Supervisor (`one_for_all`).
    * Nadzoruje proces silnika gry (`game_engine`).
3.  **`game_engine.erl`** (`gen_server`)
    * **Serce gry.** Przechowuje stan wszystkich graczy i aktywnych ataków.
    * Obsługuje pętlę gry (Tick Rate: **100ms**).
    * Implementuje logikę: poruszanie się, strzelanie, wykrywanie kolizji, naliczanie punktów.
    * Rozgłasza stan świata (broadcast) do wszystkich połączonych procesów.
4.  **`ws_handler.erl`** (`cowboy_websocket`)
    * Obsługuje pojedyncze połączenie gracza.
    * Dekoduje JSON od klienta i przekazuje komendy (`cast`) do silnika gry.
    * Odbiera stan gry z silnika i wysyła go jako JSON do klienta.

---

## 📡 Protokół Komunikacyjny (WebSocket)

### 1. Klient ➔ Serwer (Akcje)

**Ruch:**
```json
{
  "type": "move",
  "dir": "up" | "down" | "left" | "right"
}
```

**Strzał:**
```json
{
  "type": "shoot"
}
```

### 2. Serwer ➔ Klient (Eventy)
## Inicjalizacja (po dołączeniu):
```json
{
  "type": "init",
  "self_id": "binary_pid_string",
  "map_size": 100
}
```

### Stan Gry (wysyłane co 100ms):
```json
{
  "type": "state",
  "players": [
    {
      "id": "...",
      "x": 10,
      "y": 20,
      "dir": "right",
      "status": "alive",
      "score": 5
    }
  ],
  "attacks": [
    {"x": 11, "y": 20, "w": 5, "h": 1, "color": "yellow"}
  ]
}
```
### Śmierć gracza:
```json
{
  "type": "death",
  "killer_id": "...",
  "final_score": 10
}
```
## 🚀 Uruchomienie
### 1. Pobierz zależności i skompiluj projekt:
```
rebar3 compile
```
### 2. Uruchom powłokę z aplikacją:
```
rebar3 shell
```
### 3. Serwer nasłuchuje pod adresem: `ws://localhost:8080/ws`

## 🗺️ Logika Gry
- Mapa: Kwadrat 100x100 jednostek.
- Atak: Obszarowy (Area of Effect). Zasięg zależy od kierunku patrzenia gracza.
- Kolizja: Prosta detekcja prostokątna (AABB).
- Punktacja: +1 punkt za zestrzelenie przeciwnika. Zestrzelony gracz jest rozłączany.