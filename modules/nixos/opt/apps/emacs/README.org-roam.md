# Org-Roam + Org-Agenda (Doom Emacs, non-evil)

Leader: `C-c`, localleader (в org-буферах): `C-c l`.

## Структура

```
~/org/roam2/                    # org-roam-directory
├── daily/                      # Ежедневные заметки (inbox)
│   ├── 2026-02-20.org
│   └── 2026-02-21.org
├── encrypted/                  # Зашифрованные заметки (.org.gpg)
│   └── 20260220120000-salary.org.gpg
├── 20260220120000-nix-patterns.org
├── 20260220130000-project-website.org
└── ...
```

Плоская структура. Категоризация через `#+filetags:`, связи через ссылки.
Файлы именуются автоматически: `TIMESTAMP-slug.org`.

---

## ВАЖНО: org-capture vs org-roam-capture

Это **разные** команды с разными хранилищами:

| Keybinding | Команда | Куда сохраняет |
|---|---|---|
| `C-c n n` | `org-capture` | `~/org/todo.org`, `~/org/notes.org`, `~/org/journal.org` (Doom defaults) |
| `C-c n r n` | `org-roam-capture` | `~/org/roam2/*.org` (roam-заметки) |
| `C-c n r d n` | `org-roam-dailies-capture-today` | `~/org/roam2/daily/2026-02-21.org` (daily inbox) |

`org-capture` (`C-c n n`) — это стандартный Doom capture в простые org-файлы.
**Для roam-заметок всегда используй `C-c n r ...`** (префикс `r` = roam).

---

## Keybindings

### Глобальные: Notes (`C-c n`)

| Keybinding | Команда | Описание |
|---|---|---|
| `C-c n a` | `org-agenda` | Открыть agenda dispatcher |
| `C-c n n` | `org-capture` | Быстрый захват (org-capture) |
| `C-c n N` | `org-capture-goto-target` | Перейти к целевому файлу capture |
| `C-c n l` | `org-store-link` | Сохранить ссылку на текущую позицию |
| `C-c n t` | `org-todo-list` | Список всех TODO |
| `C-c n m` | `org-tags-view` | Поиск по тегам |
| `C-c n s` | `+default/org-notes-search` | Полнотекстовый поиск в заметках |
| `C-c n S` | `+default/org-notes-headlines` | Поиск по заголовкам agenda |
| `C-c n f` | `+default/find-in-notes` | Найти файл в заметках |
| `C-c n F` | `+default/browse-notes` | Обзор заметок (dired) |
| `C-c n v` | `org-search-view` | Поиск по org-файлам |
| `C-c n c` | `+org/toggle-last-clock` | Переключить последний clock |
| `C-c n C` | `org-clock-cancel` | Отменить текущий clock |
| `C-c n o` | `org-clock-goto` | Перейти к активному clock |
| `C-c n .` | `+default/search-notes-for-symbol-at-point` | Искать символ под курсором |

### Org-Roam (`C-c n r`)

| Keybinding | Команда | Описание |
|---|---|---|
| `C-c n r f` | `org-roam-node-find` | Найти/создать roam-заметку |
| `C-c n r F` | `org-roam-ref-find` | Найти заметку по ref |
| `C-c n r i` | `org-roam-node-insert` | Вставить ссылку на roam-заметку |
| `C-c n r n` | `org-roam-capture` | Захватить новую roam-заметку |
| `C-c n r r` | `org-roam-buffer-toggle` | Показать/скрыть backlinks панель |
| `C-c n r R` | `org-roam-buffer-display-dedicated` | Открыть backlinks в отдельном буфере |
| `C-c n r g` | `org-roam-graph` | Граф связей заметок |
| `C-c n r s` | `org-roam-db-sync` | Синхронизировать БД |
| `C-c n r a` | `org-roam-node-random` | Случайная заметка |

### Org-Roam Dailies (`C-c n r d`)

| Keybinding | Команда | Описание |
|---|---|---|
| `C-c n r d n` | `org-roam-dailies-capture-today` | Захват в сегодняшнюю daily |
| `C-c n r d T` | `org-roam-dailies-capture-today` | Захват в сегодняшнюю daily (дубль) |
| `C-c n r d t` | `org-roam-dailies-goto-today` | Открыть сегодняшнюю daily |
| `C-c n r d y` | `org-roam-dailies-goto-yesterday` | Открыть вчерашнюю daily |
| `C-c n r d Y` | `org-roam-dailies-capture-yesterday` | Захват во вчерашнюю daily |
| `C-c n r d m` | `org-roam-dailies-goto-tomorrow` | Открыть завтрашнюю daily |
| `C-c n r d M` | `org-roam-dailies-capture-tomorrow` | Захват в завтрашнюю daily |
| `C-c n r d d` | `org-roam-dailies-goto-date` | Открыть daily по дате |
| `C-c n r d D` | `org-roam-dailies-capture-date` | Захват в daily по дате |
| `C-c n r d b` | `org-roam-dailies-goto-previous-note` | Предыдущая daily |
| `C-c n r d f` | `org-roam-dailies-goto-next-note` | Следующая daily |
| `C-c n r d -` | `org-roam-dailies-find-directory` | Открыть каталог dailies |

### В org-буфере: localleader (`C-c l`)

| Keybinding | Команда | Описание |
|---|---|---|
| `C-c l t` | `org-todo` | Сменить TODO-статус |
| `C-c l T` | `org-todo-list` | Список всех TODO |
| `C-c l q` | `org-set-tags-command` | Установить теги |
| `C-c l d d` | `org-deadline` | Установить deadline |
| `C-c l d s` | `org-schedule` | Запланировать дату |
| `C-c l d t` | `org-time-stamp` | Вставить timestamp |
| `C-c l n` | `org-store-link` | Сохранить ссылку |
| `C-c l l l` | `org-insert-link` | Вставить ссылку |
| `C-c l l s` | `org-store-link` | Сохранить ссылку |
| `C-c l e` | `org-export-dispatch` | Экспорт |
| `C-c l .` | `consult-org-heading` | Перейти к заголовку (vertico) |
| `C-c l /` | `consult-org-agenda` | Поиск по agenda-заголовкам |
| `C-c l I` | `org-id-get-create` | Создать ID для заголовка |

### В org-буфере: Roam localleader (`C-c l m`)

| Keybinding | Команда | Описание |
|---|---|---|
| `C-c l m f` | `org-roam-node-find` | Найти/создать roam-заметку |
| `C-c l m i` | `org-roam-node-insert` | Вставить ссылку на заметку |
| `C-c l m n` | `org-roam-capture` | Захватить новую заметку |
| `C-c l m m` | `org-roam-buffer-toggle` | Показать/скрыть backlinks |
| `C-c l m M` | `org-roam-buffer-display-dedicated` | Backlinks в отдельном буфере |
| `C-c l m g` | `org-roam-graph` | Граф связей |
| `C-c l m r` | `org-roam-refile` | Refile в другую roam-заметку |
| `C-c l m R` | `org-roam-link-replace-all` | Заменить все roam-ссылки |
| `C-c l m D` | `org-roam-demote-entire-buffer` | Понизить уровень всего буфера |
| `C-c l m F` | `org-roam-ref-find` | Найти по ref |
| `C-c l m I` | `org-id-get-create` | Создать ID |
| `C-c l m o a` | `org-roam-alias-add` | Добавить alias |
| `C-c l m o t` | `org-roam-tag-add` | Добавить тег |
| `C-c l m o r` | `org-roam-ref-add` | Добавить ref |

### В org-буфере: Refile (`C-c l r`)

| Keybinding | Команда | Описание |
|---|---|---|
| `C-c l r r` | `org-refile` | Refile заголовок |
| `C-c l r .` | `+org/refile-to-current-file` | Refile в текущий файл |
| `C-c l r f` | `+org/refile-to-file` | Refile в другой файл |
| `C-c l r o` | `+org/refile-to-other-window` | Refile в другое окно |
| `C-c l r l` | `+org/refile-to-last-location` | Refile в последнее место |
| `C-c l r c` | `+org/refile-to-running-clock` | Refile к активному clock |

### В буфере Agenda

| Keybinding | Команда | Описание |
|---|---|---|
| `RET` | `org-agenda-switch-to` | Открыть задачу |
| `C-c l t` / `t` | `org-agenda-todo` | Сменить TODO-статус |
| `C-c l d s` | `org-agenda-schedule` | Запланировать дату |
| `C-c l d d` | `org-agenda-deadline` | Установить deadline |
| `C-c l p p` | `org-agenda-priority` | Установить приоритет |
| `q` | `org-agenda-set-tags` | Установить теги |
| `C-c l c i` | `org-agenda-clock-in` | Clock in задачу |
| `C-c l c o` | `org-agenda-clock-out` | Clock out |

### Custom Agenda Views

| Keybinding | Описание |
|---|---|
| `C-c n a` затем `d` | **Dashboard**: день + TODO + WAIT + STRT из roam |
| `C-c n a` затем `r` | **Roam TODOs**: все задачи из roam-заметок |
| `C-c n a` затем `t` | **Todo list**: все TODO (стандартный) |

---

## Воркфлоу: работа с заметками

### Создание заметки

#### Быстрая заметка (мысль, идея, факт)

1. `C-c n r d n` (`org-roam-dailies-capture-today`) -> `d` (default)
2. Написать текст
3. `C-c C-c` — сохранить

Сохраняется в `~/org/roam2/daily/2026-02-21.org` как новый заголовок.
Daily — это inbox: сюда пишешь всё подряд, потом разбираешь.

#### Полноценная заметка (тема, концепт, документация)

1. `C-c n r n` (`org-roam-capture`) -> `d` (default)
2. Ввести заголовок (станет именем файла и `#+title:`)
3. Написать содержимое. Добавить теги в `#+filetags:`
4. `C-c C-c` — сохранить

Сохраняется в `~/org/roam2/20260221143000-slug.org`.

#### Задача (TODO)

1. `C-c n r n` (`org-roam-capture`) -> `t` (task)
2. Ввести заголовок
3. Написать детали (курсор уже на `* TODO`, внизу — timestamp и ссылка на место вызова)
4. `C-c C-c` — сохранить

Заметка автоматически получит тег `:project:` (vulpea) и появится в agenda.

#### Задача со ссылкой на текущий файл

Шаблон `t` включает `%a` — ссылку на место, откуда вызван capture.

1. Открыть любой файл (код, org, markdown...), встать на нужную строку
2. `C-c n r n` (`org-roam-capture`) -> `t` (task)
3. `C-c C-c` — сохранить

Результат:
```org
* TODO Исправить баг в парсере
  [2026-02-21 Fri 14:30]
  [[file:~/work/project/parser.py::42][parser.py]]
```

То же работает из daily: `C-c n r d n` -> `t`.

#### Быстрая задача в inbox

1. `C-c n r d n` (`org-roam-dailies-capture-today`) -> `t` (task)
2. Написать задачу
3. `C-c C-c` — сохранить
4. Позже — refile в проектную заметку (см. "Разбор inbox")

#### Секретная заметка (шифрованная)

1. `C-c n r n` (`org-roam-capture`) -> `s` (secret)
2. Ввести заголовок
3. Написать содержимое
4. `C-c C-c` — файл создан в `~/org/roam2/encrypted/*.org.gpg`, зашифрован GPG

При открытии — автоматическая расшифровка (pinentry запросит passphrase).

Для шифрования отдельных секций в обычных файлах — добавить тег `:crypt:`:
```org
* Обычная секция
  Открытый текст.

* Секретная секция                                               :crypt:
  Этот текст зашифруется при сохранении (C-x C-s).
```
Расшифровать: `M-x org-decrypt-entry` / `M-x org-decrypt-entries`.

---

### Сохранение заметки

- `C-c C-c` — завершить capture (сохранить и закрыть capture-буфер)
- `C-c C-k` — отменить capture (не сохранять)
- `C-x C-s` — сохранить в обычном org-буфере (не capture)

При сохранении roam-заметки с TODO — vulpea автоматически добавит `:project:` тег.

---

### Поиск заметки

| Что ищешь | Keybinding | Команда | Как |
|---|---|---|---|
| По заголовку | `C-c n r f` | `org-roam-node-find` | Ввести часть заголовка (fuzzy) |
| По тегу | `C-c n r f` | `org-roam-node-find` | Ввести `#tag` (напр. `#dev`) |
| Полнотекстовый | `C-c n s` | `+default/org-notes-search` | Ripgrep по `~/org/` |
| По заголовкам org | `C-c n S` | `+default/org-notes-headlines` | Поиск по headlines |
| По org-тегам | `C-c n m` | `org-tags-view` | Ввести тег (`:work:` и т.д.) |
| Backlinks | `C-c l m m` | `org-roam-buffer-toggle` | Панель справа в roam-заметке |
| Граф связей | `C-c n r g` | `org-roam-graph` | Визуальный граф |
| Случайная | `C-c n r a` | `org-roam-node-random` | Для ревизии старых заметок |

**Типичный сценарий**: "где я писал про NixOS overlay?"
-> `C-c n r f`, вводишь `overlay` или `#nix` — находишь заметку.

**Типичный сценарий**: "что ссылается на эту заметку?"
-> Открыть заметку, `C-c l m m` — справа появится список backlinks.

---

### Agenda: обзор задач

Agenda собирает все TODO из roam-заметок автоматически (vulpea).

#### Утренний обзор

`C-c n a` (`org-agenda`) -> `d` (Dashboard):
- Расписание на сегодня (scheduled/deadline)
- Все TODO из roam-заметок
- Задачи в ожидании (WAIT)
- Задачи в работе (STRT)

#### Другие представления

| Ключ | Представление |
|---|---|
| `d` | Dashboard (день + TODO + WAIT + STRT) |
| `r` | Все roam-заметки с TODO |
| `t` | Стандартный список TODO |
| `m` | Поиск по тегам |
| `s` | Поиск по тексту |

#### Работа в буфере agenda

- `RET` — открыть задачу
- `t` — сменить TODO-статус (TODO -> STRT -> DONE)
- `C-c l d s` — запланировать дату (schedule)
- `C-c l d d` — установить deadline
- `C-c l p p` — установить приоритет
- `C-c l c i` / `C-c l c o` — clock in / clock out

---

### Связывание заметок

В roam-заметке:
1. `C-c l m i` (`org-roam-node-insert`) — вставить ссылку на другую заметку
2. Начать вводить название — vertico покажет варианты
3. Выбрать — вставляется `[[id:...][Title]]`

Backlinks: `C-c l m m` (`org-roam-buffer-toggle`) — панель справа со всеми заметками, ссылающимися на текущую.

---

### Разбор inbox (dailies)

Вечером или когда накопилось:

1. `C-c n r d t` (`org-roam-dailies-goto-today`) — открыть сегодняшнюю daily
2. Для каждой записи — решить что с ней:

| Действие | Как |
|---|---|
| Создать отдельную roam-заметку | `C-c n r n` (`org-roam-capture`) и перенести текст |
| Refile в существующую заметку | Курсор на заголовок, `C-c l r f` (`+org/refile-to-file`) |
| Refile в другую roam-ноду | Курсор на заголовок, `C-c l m r` (`org-roam-refile`) |
| Добавить ссылку на roam-ноду | `C-c l m i` (`org-roam-node-insert`) |
| Пометить как выполненное | `C-c l t` (`org-todo`) -> DONE |
| Удалить | Выделить дерево, `C-c l s d` (`org-cut-subtree`) |

---

### Ежедневный ритм (рекомендация)

```
Утро:    C-c n a -> d          Обзор Dashboard
         ├── Посмотреть scheduled/deadline на сегодня
         └── Выбрать задачи на день (STRT)

День:    C-c n r d n -> d/t    Быстрые записи в daily inbox
         C-c n r n -> t        Новые задачи со ссылкой на контекст
         C-c n r f             Открыть заметку для работы

Вечер:   C-c n r d t           Разбор inbox
         ├── Refile задачи в проекты
         ├── Создать отдельные заметки из идей
         └── DONE завершённое

Неделя:  C-c n a -> r          Обзор всех roam TODO
         C-c n r a             Случайная заметка (ревизия)
```

---

## TODO-статусы

```
TODO -> STRT -> DONE
     -> WAIT
     -> DLGT
     -> CANC
```

| Статус | Значение |
|---|---|
| **TODO** | Запланировано |
| **STRT** | В работе |
| **WAIT** | Ожидание (кого-то/чего-то) |
| **DLGT** | Делегировано |
| **DONE** | Завершено |
| **CANC** | Отменено |

Переключение: `C-c l t` (`org-todo`) в org-файле.

---

## Vulpea: автоматическая интеграция Agenda + Roam

Не нужно вручную управлять `org-agenda-files`. Система работает автоматически:

1. Открываешь/сохраняешь roam-заметку с любым TODO
2. Vulpea добавляет `#+filetags: :project:` в файл
3. При открытии agenda — собираются все файлы с тегом `project`
4. Когда все TODO завершены/удалены — тег `project` убирается

**Итого**: просто пиши TODO в roam-заметках, они автоматически появятся в agenda.

---

## Теги (filetags)

Рекомендуемые теги для категоризации:

| Тег | Назначение | Автоматический? |
|---|---|---|
| `:project:` | Файл с открытыми TODO | Да (vulpea) |
| `:daily:` | Ежедневная заметка | Да (capture template) |
| `:secret:` | Зашифрованная заметка | Да (capture template) |
| `:task:` | Заметка-задача | Да (capture template) |
| `:dev:` | Разработка | Нет |
| `:nix:` | NixOS | Нет |
| `:emacs:` | Emacs | Нет |
| `:work:` | Рабочее | Нет |
| `:personal:` | Личное | Нет |
| `:finance:` | Финансы | Нет |
| `:health:` | Здоровье | Нет |
| `:learning:` | Обучение | Нет |
| `:idea:` | Идеи | Нет |
| `:reference:` | Справочный материал | Нет |

Комбинирование: `#+filetags: :dev:nix:project:`

Фильтр в `org-roam-node-find`: ввести `#dev` для поиска по тегу.

---

## Capture Templates

### Roam Capture (`C-c n r n` / `org-roam-capture`)

| Ключ | Название | Описание |
|---|---|---|
| `d` | default | Обычная roam-заметка в `~/org/roam2/` |
| `t` | task | Заметка с `* TODO` + timestamp + ссылка на источник (`%a`) |
| `s` | secret | Зашифрованная заметка в `~/org/roam2/encrypted/*.org.gpg` |

### Daily Capture (`C-c n r d n` / `org-roam-dailies-capture-today`)

| Ключ | Название | Описание |
|---|---|---|
| `d` | default | Запись в daily (`* заметка`) |
| `t` | task | Задача в daily (`* TODO` + timestamp + ссылка на источник) |

---

## Шифрование

### GPG-ключ

Используется ключ `B33D6CC4C3A13CF7` (тот же что для git signing).

### Полное шифрование файла (.org.gpg)

- Файлы в `~/org/roam2/encrypted/*.org.gpg`
- EasyPG: автоматическое шифрование/расшифровка при сохранении/открытии
- Org-roam индексирует `.org.gpg` как обычные заметки

### Шифрование заголовков (org-crypt)

- Тег `:crypt:` на заголовке -> содержимое шифруется при `C-x C-s`
- `M-x org-decrypt-entry` — расшифровать один заголовок
- `M-x org-decrypt-entries` — расшифровать все
- Тег `:crypt:` не наследуется дочерними заголовками

---

## Persistence

- `~/org/` персистируется как **state** (с бэкапом), включая `~/org/roam2/`
- БД `org-roam.db` в `~/.config/emacs/` (derivative) — пересоздаётся через `C-c n r s` (`org-roam-db-sync`)
