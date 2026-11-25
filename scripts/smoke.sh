#!/usr/bin/env bash
# Наскрізна перевірка виконуваного файлу на прикладах з examples/.
set -euo pipefail
cd "$(dirname "$0")/.."

cabal build exe:textbook-word-replacer >/dev/null
BIN="$(cabal list-bin exe:textbook-word-replacer)"
PASS=0

ok() { PASS=$((PASS + 1)); echo "PASS $1"; }
fail() { echo "FAIL $1" >&2; exit 1; }

# 1. Коротша заміна (українська), локаль C — перевірка UTF-8.
out="$(LC_ALL=C "$BIN" -s 6 -l 5 -r "ЇЖ" --text-only examples/textbook_uk.txt)"
grep -q "У Haskell ЇЖ змінних у звичному розумінні, є лише значення та ЇЖ\." <<<"$out" || fail "коротша заміна"
ok "коротша заміна (UTF-8 при LC_ALL=C)"

# 2. Довша заміна, розділові знаки на місці.
out="$("$BIN" -s 7 -l 4 -r "[LONGER]" --text-only examples/textbook_en.txt)"
grep -q "^The value 2.718 is called e, and the identifier snake_case is a normal name\." <<<"$out" \
  && fail "речення 7 мало змінитись"
grep -q "a normal \[LONGER\]\." <<<"$out" || fail "довша заміна"
ok "довша заміна, крапка на місці"

# 3. Порожня заміна.
out="$("$BIN" -s 4 -l 6 --replace= examples/textbook_en.txt)"
grep -q "^Стало: Not, e.g. when you start with small programs!$" <<<"$out" || fail "порожня заміна"
ok "порожня заміна"

# 4. Брудні пробіли нормалізуються.
out="$("$BIN" -s 1 -l 7 -r "X" --text-only examples/messy.txt)"
grep -q "^Мова X є функціональною\. Вона має сильну типізацію! Чи це добре? Так, зазвичай добре\.\.\. але не завжди\. Кінець тексту без крапки$" <<<"$out" \
  || fail "нормалізація пробілів: $out"
ok "нормалізація пробілів у messy.txt"

# 5. Інтерактивний режим.
out="$(printf 'examples/simple.txt\n2\n5\nXX\n' | "$BIN")"
grep -q "^Стало: XX речення з кількома словами!$" <<<"$out" || fail "інтерактивний режим: $out"
ok "інтерактивний режим"

# 6. Помилкові дані: код виходу 1 і зрозуміле повідомлення.
set +e
out="$("$BIN" -s 99 -l 3 -r x examples/simple.txt 2>&1)"; code=$?
set -e
[[ $code -eq 1 ]] && grep -q "речення з номером 99 не існує" <<<"$out" || fail "помилковий номер речення"
ok "помилковий номер речення"

set +e
out="$("$BIN" -s 1 -l 0 -r x examples/simple.txt 2>&1)"; code=$?
set -e
[[ $code -eq 1 ]] && grep -q "довжина слова має бути додатним" <<<"$out" || fail "помилкова довжина"
ok "помилкова довжина"

set +e
out="$("$BIN" -s 1 -l 3 -r x no-such-file.txt 2>&1)"; code=$?
set -e
[[ $code -eq 1 ]] && grep -q "не вдалося прочитати файл" <<<"$out" || fail "відсутній файл"
ok "відсутній файл"

echo "Усього пройдено: $PASS"
