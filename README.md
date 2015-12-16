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
