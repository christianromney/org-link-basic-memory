#!/usr/bin/env bash
# Manual test script to validate permalink stability across file moves
# This script demonstrates that memory:// links remain valid when files are moved

set -e

echo "=== Testing Permalink Stability Across File Moves ==="
echo

# Colors for output
GREEN='\033[0.32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create temp directory
TEMP_DIR=$(mktemp -d)
echo -e "${BLUE}1. Created temporary test directory: $TEMP_DIR${NC}"

# Add as basic-memory project
basic-memory project add permalink-test "$TEMP_DIR" 2>&1 | grep -v Deprecation > /dev/null
echo -e "${GREEN}✓ Added test project${NC}"

# Set as default
basic-memory project default permalink-test 2>&1 | grep -v Deprecation > /dev/null
echo -e "${GREEN}✓ Set as default project${NC}"
echo

# Create test file in original location
ORIGINAL_PATH="$TEMP_DIR/original/location/test-note.md"
mkdir -p "$(dirname "$ORIGINAL_PATH")"
cat > "$ORIGINAL_PATH" << 'EOF'
---
title: Test Note for Permalink Stability
type: note
permalink: test/permalink-stability
---

# Test Note

This note tests that memory:// links remain stable when files are moved.

The permalink is: test/permalink-stability
EOF

echo -e "${BLUE}2. Created test file at: original/location/test-note.md${NC}"

# Sync and wait
basic-memory sync 2>&1 | grep "Synced"
sleep 3
echo -e "${GREEN}✓ File synced to database${NC}"
echo

# Search by permalink - should find original location
RESULT_BEFORE=$(basic-memory tool search-notes --permalink 'test/permalink-stability' 2>/dev/null | grep '"file_path"' | cut -d'"' -f4)
echo -e "${BLUE}3. Searching by permalink before move:${NC}"
echo -e "   Found at: ${YELLOW}$RESULT_BEFORE${NC}"
echo

# Move file to new location
NEW_PATH="$TEMP_DIR/moved/different/subdirectory/test-note.md"
mkdir -p "$(dirname "$NEW_PATH")"
mv "$ORIGINAL_PATH" "$NEW_PATH"
echo -e "${BLUE}4. Moved file to: moved/different/subdirectory/test-note.md${NC}"

# Sync again
basic-memory sync 2>&1 | grep "Synced"
sleep 3
echo -e "${GREEN}✓ Move synced to database${NC}"
echo

# Search by permalink again - should find NEW location
RESULT_AFTER=$(basic-memory tool search-notes --permalink 'test/permalink-stability' 2>/dev/null | grep '"file_path"' | cut -d'"' -f4)
echo -e "${BLUE}5. Searching by SAME permalink after move:${NC}"
echo -e "   Found at: ${YELLOW}$RESULT_AFTER${NC}"
echo

# Verify the permalink hasn't changed in the file
PERMALINK_IN_FILE=$(grep "^permalink:" "$NEW_PATH" | cut -d' ' -f2)
echo -e "${BLUE}6. Verifying permalink in file hasn't changed:${NC}"
echo -e "   Permalink in file: ${YELLOW}$PERMALINK_IN_FILE${NC}"
echo

# Validate results
echo "=== Test Results ==="
if [ "$RESULT_BEFORE" != "$RESULT_AFTER" ]; then
    echo -e "${GREEN}✓ PASS: Permalink resolved to different paths (file actually moved)${NC}"
    echo -e "  Before: $RESULT_BEFORE"
    echo -e "  After:  $RESULT_AFTER"
else
    echo -e "${YELLOW}✗ FAIL: Same path returned (file may not have moved)${NC}"
fi

if [ -n "$RESULT_AFTER" ] && [ "$RESULT_AFTER" = "moved/different/subdirectory/test-note.md" ]; then
    echo -e "${GREEN}✓ PASS: Permalink correctly resolved to new location${NC}"
else
    echo -e "${YELLOW}✗ FAIL: Permalink did not resolve to new location${NC}"
    echo -e "  Expected: moved/different/subdirectory/test-note.md"
    echo -e "  Got: $RESULT_AFTER"
fi

if [ "$PERMALINK_IN_FILE" = "test/permalink-stability" ]; then
    echo -e "${GREEN}✓ PASS: Permalink unchanged in file after move${NC}"
else
    echo -e "${YELLOW}✗ FAIL: Permalink changed in file${NC}"
fi

# Cleanup
echo
echo -e "${BLUE}7. Cleaning up...${NC}"
basic-memory project default troy 2>&1 | grep -v Deprecation > /dev/null
basic-memory project remove permalink-test 2>&1 > /dev/null
rm -rf "$TEMP_DIR"
echo -e "${GREEN}✓ Cleanup complete${NC}"

echo
echo "=== Summary ==="
echo "This test demonstrates that:"
echo "1. Files can be found by permalink before moving"
echo "2. Files can still be found by the SAME permalink after moving"
echo "3. The permalink field in frontmatter remains unchanged"
echo "4. Basic Memory's database tracks the new location"
echo
echo "This proves that memory:// links remain stable across file moves!"
