<?php

declare(strict_types=1);

function linkItem(string $href, string $label): string
{
    return '<li><a href="' . htmlspecialchars($href, ENT_QUOTES, 'UTF-8') . '">' . htmlspecialchars($label, ENT_QUOTES, 'UTF-8') . '</a></li>';
}

?><!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Test Runner</title>
  <style>
    body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; background:#0f172a; color:#e2e8f0; margin:0; }
    .container { max-width: 760px; margin: 40px auto; padding: 0 16px; }
    .card { background:#111827; padding:20px; border:1px solid #1f2937; border-radius:10px; }
    a { color:#60a5fa; text-decoration:none; }
    a:hover { text-decoration:underline; }
  </style>
  </head>
<body>
  <div class="container">
    <h1>Test Runner</h1>
    <div class="card">
      <p>Run the following test suites:</p>
      <ul>
        <?php
          echo linkItem('/tests/test_soal1.php', 'Test Problem 1 - Sorting');
          echo linkItem('/tests/test_soal2.php', 'Test Problem 2 - Diamond');
        ?>
      </ul>
      <p>Solutions:</p>
      <ul>
        <?php
          echo linkItem('/soal1.php', 'Problem 1 Output');
          echo linkItem('/soal2.php', 'Problem 2 Output');
        ?>
      </ul>
    </div>
  </div>
</body>
</html>


