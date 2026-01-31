#!/bin/bash
# run_tests.sh - Run larj-model tests and generate visualizations
#
# Usage: ./scripts/run_tests.sh [--visualize]
#
# Options:
#   --visualize  Also generate PNG/SVG visualizations (requires Graphviz)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$PROJECT_DIR/output"

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo "=== larj-model Test Runner ==="
echo "Project: $PROJECT_DIR"
echo "Output: $OUTPUT_DIR"
echo ""

# Check for SWI-Prolog
if ! command -v swipl &> /dev/null; then
    echo "ERROR: SWI-Prolog (swipl) not found. Please install it."
    exit 1
fi

echo "Running unit tests..."
cd "$PROJECT_DIR"

# Run the schema unit tests
swipl -g "
    [test/test_schema],
    run_tests,
    halt(0)
" -t "halt(1)" 2>&1 | tee "$OUTPUT_DIR/unit_test_results.txt"

echo ""
echo "Running SDD requirement tests..."

# Run the SDD requirement-mapped tests
swipl -g "
    [test/test_requirements],
    run_tests,
    halt(0)
" -t "halt(1)" 2>&1 | tee "$OUTPUT_DIR/sdd_test_results.txt"

echo ""
echo "Running validation tests..."

# Run the validation tests
swipl -g "
    [test/test_ecommerce_validation],
    run_all_validation_tests,
    halt(0)
" -t "halt(1)" 2>&1 | tee -a "$OUTPUT_DIR/test_results.txt"

echo ""
echo "Test results saved to:"
echo "  - $OUTPUT_DIR/unit_test_results.txt"
echo "  - $OUTPUT_DIR/sdd_test_results.txt"
echo "  - $OUTPUT_DIR/test_results.txt"

# Generate DOT files
echo ""
echo "Generating DOT visualization files..."
swipl -g "
    [examples/ecommerce_model],
    [src/model_visualize],
    build_ecommerce_model,
    generate_dot_file(ecommerce, '$OUTPUT_DIR/ecommerce_model.dot', []),
    generate_context_map_dot(ecommerce, CtxMapDot),
    open('$OUTPUT_DIR/ecommerce_context_map.dot', write, S1),
    write(S1, CtxMapDot),
    close(S1),
    generate_context_dot_file(order_ctx, ecommerce, '$OUTPUT_DIR/order_context.dot'),
    halt(0)
" -t "halt(1)"

echo "Generated DOT files:"
echo "  - $OUTPUT_DIR/ecommerce_model.dot"
echo "  - $OUTPUT_DIR/ecommerce_context_map.dot"
echo "  - $OUTPUT_DIR/order_context.dot"

# Optionally render to PNG/SVG
if [[ "$1" == "--visualize" ]]; then
    echo ""
    echo "Rendering visualizations..."

    if ! command -v dot &> /dev/null; then
        echo "WARNING: Graphviz (dot) not found. Skipping PNG/SVG generation."
        echo "Install Graphviz to generate images: brew install graphviz"
    else
        for dotfile in "$OUTPUT_DIR"/*.dot; do
            basename="${dotfile%.dot}"
            echo "  Rendering: $dotfile"
            dot -Tpng "$dotfile" -o "${basename}.png"
            dot -Tsvg "$dotfile" -o "${basename}.svg"
        done
        echo ""
        echo "Generated images:"
        ls -la "$OUTPUT_DIR"/*.png "$OUTPUT_DIR"/*.svg 2>/dev/null || true
    fi
fi

echo ""
echo "=== Done ==="
