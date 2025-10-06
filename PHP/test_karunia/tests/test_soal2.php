<?php

declare(strict_types=1);

// Reuse logic by including and capturing output to parse DOM-free checks
ob_start();
require __DIR__ . '/../soal2.php';
$html = ob_get_clean();

function assert_true(bool $cond, string $message): void
{
    if (!$cond) {
        throw new RuntimeException($message);
    }
}

// Extract radius and grid size from meta text
// Example snippet: "Random radius: <strong>12</strong> • Grid: 25×25"
preg_match('/Random radius:\s*<strong>(\d+)<\/strong>\s*•\s*Grid:\s*(\d+)×\2/i', $html, $m);
assert_true(!empty($m), 'Meta info (radius/grid) not found');

$radius = (int)$m[1];
$size = (int)$m[2];
assert_true($radius >= 6 && $radius <= 14, 'Radius out of range 6..14');
assert_true($size === $radius * 2 + 1, 'Grid size not equal to 2*radius+1');

// Count filled cells (edge count of Manhattan circle/diamond) = 4*radius
preg_match_all('/class=\\"cell fill\\"/', $html, $filled);
$filledCount = count($filled[0]);
assert_true($filledCount === 4 * $radius, 'Edge cell count is not 4*radius');

header('Content-Type: text/plain; charset=UTF-8');
echo "PASS: All Problem 2 tests passed\n";


