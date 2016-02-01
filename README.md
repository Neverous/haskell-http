# Haskell-HTTP

Prosty serwer HTTP napisany w haskellu.

## Specyfikacja (podstawowa)

* Serwowanie plików statycznych
* Prosty plik konfiguracyjny (główna ścieżka, liczba workerów itp.)
* Obsługa HTTP/1.1
* Własny parser nagłówków - bez wykorzystywania zewnętrznych bibliotek/frameworków do parsowania
* Ręczna obsługa połączeń, socketów

## Rozszerzenie

* Serwowanie plików CGI (to nie powinno być specjalnie trudne)
* Obsługa WSGI (to już trochę bardziej skomplikowane)
* HTTP/2.0 (wątpliwe)
* Obsługa wielu ścieżek, wyrażenia regularne na ścieżki itd.

## Jak skompilować i uruchomić
Wymagane biblioteki MTL, Network

### Kompilacja
Wystarczy w katalogu `src/` wywołać make

### Uruchomienie
Przykładowa konfiguracja (wraz z przykładową stroną) znajduje się w katalogu example

Aby uruchomić należy albo zamienić document\_root w pliku config.ini tak aby wskazywał na example/www (wymagana pełna ścieżka),
albo skopiować example/www do /tmp/

Jak już mamy ustawioną konfigurację to aby uruchomić serwer wystarczy:
```sh
$ ./hhttp -c ../example/config.ini -v debug +RTS -N2
```

gdzie:
* flaga `-c` pozwala wybrać plik konfiguracyjny (domyślnie config.ini w katalogu uruchomienia)
* flaga `-v` pozwala wybrać poziom informacji diagnostycznych `debug,notice,info,warning,error`
* flagi `+RTS -N2` to ustawienia wielowątkowości z haskella
* flaga `-h` wyświetla powyższe informacje (poza wielowątkowością)

## Plik konfiguracyjny
Konfiguracja jest prostym plikiem o zawartości jak pliki ini.

Składa się z sekcji, które zawierają opcje.
```ini
[sekcja]
opcja1 = wartość1
```

### Global
Sekcja `global` zawiera ogólną konfigurację:
```ini
[global]
; komentarz
    host            = 0.0.0.0
    port            = 8080
    server_name     = localhost
    server_string   = hhttp 0.1
    document_root   = /var/www/public/
    verbosity       = 1

```

gdzie:
* **host** - adres na którym serwer będzie nasłuchiwał
* **port** - port jw.
* **server_name** - nazwa pod jaką serwer będzie odpowiadał
* **server_string** - nazwa serwera zwracana do klientów
* **document_root** - ścieżka do plików dla domyślnej ścieżki `/`
* **verbosity** - poziom informacji diagnostycznych 1-6 [najwięcej informacji - żadne]

### Ścieżki
Sekcja `path:/XXXX/` zawiera konfigurację dla ścieżki `/XXXX/`
```ini
[path:/admin/]
    document_root   = /var/www/
```

gdzie:
* **document_root** - ścieżka do plików dla adresów zaczynających się daną ścieżka (w tym przypadku `/admin/` - szukamy plików w /var/www/admin/xxx dla http://adres/admin/xxx)

