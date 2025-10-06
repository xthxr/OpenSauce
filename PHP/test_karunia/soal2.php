<?php

declare(strict_types=1);

/**
 * Problem 2
 * Render a hollow diamond shape with a random size on each refresh.
 *
 * Technique: Manhattan distance on a grid (r, c) from the center.
 * - Edge cells satisfy |r-center| + |c-center| == radius
 * - Symmetric and not dependent on ASCII boxes.
 */

function generate_random_radius(): int
{
    // Random radius between 6 and 14 (inclusive)
    return random_int(6, 14);
}

$radius = generate_random_radius();
$size = $radius * 2 + 1; // grid size (odd)
$center = $radius;       // center index

// Minimal inline CSS for clear visuals
$style = <<<CSS
<style>
  body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Arial, sans-serif; background:#0f172a; color:#e2e8f0; margin:0; }
  .container { min-height: 100vh; display:flex; flex-direction:column; align-items:center; justify-content:center; gap:16px; }
  .meta { color:#94a3b8; font-size:14px; }
  .grid { display:grid; grid-template-columns: repeat({$size}, 18px); grid-auto-rows: 18px; gap: 3px; }
  .cell { width:18px; height:18px; display:flex; align-items:center; justify-content:center; }
  .fill { background: yellowgreen; box-shadow: 0 0 6px rgba(154,205,50,0.7); border-radius:3px; }
  .empty { opacity: .15; }
  .legend { font-size:13px; color:#a3a3a3; }
  .card { background:#111827; padding:16px 20px; border:1px solid #1f2937; border-radius:10px; }
</style>
CSS;

echo $style;

echo '<div class="container">';
echo '<div class="card">';
echo '<div class="meta">Hollow Diamond • Random radius: <strong>' . $radius . '</strong> • Grid: ' . $size . '×' . $size . '</div>';
echo '<div class="grid">';

for ($r = 0; $r < $size; $r++) {
    for ($c = 0; $c < $size; $c++) {
        $isEdge = (abs($r - $center) + abs($c - $center) === $radius);
        $class = $isEdge ? 'cell fill' : 'cell empty';
        echo '<div class="' . $class . '"></div>';
    }
}

echo '</div>'; // .grid
echo '<div class="legend">Refresh the page to see a different size.</div>';
echo '</div>'; // .card
echo '</div>'; // .container


