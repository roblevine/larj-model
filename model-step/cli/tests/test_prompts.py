"""Unit tests for prompts module."""

import pytest
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from prompts import SYSTEM_PROMPT, USER_PROMPT_TEMPLATE


class TestSystemPrompt:
    """Tests for the system prompt."""

    def test_system_prompt_exists(self):
        """System prompt should be defined."""
        assert SYSTEM_PROMPT is not None
        assert len(SYSTEM_PROMPT) > 0

    def test_system_prompt_contains_archetypes(self):
        """System prompt should describe all four archetypes."""
        assert "Moment-Interval" in SYSTEM_PROMPT
        assert "Role" in SYSTEM_PROMPT
        assert "Party/Place/Thing" in SYSTEM_PROMPT
        assert "Description" in SYSTEM_PROMPT

    def test_system_prompt_contains_colors(self):
        """System prompt should mention archetype colors."""
        assert "Pink" in SYSTEM_PROMPT
        assert "Yellow" in SYSTEM_PROMPT
        assert "Green" in SYSTEM_PROMPT
        assert "Blue" in SYSTEM_PROMPT

    def test_system_prompt_contains_prolog_examples(self):
        """System prompt should contain Prolog code examples."""
        assert "define_context" in SYSTEM_PROMPT
        assert "define_moment_interval" in SYSTEM_PROMPT
        assert "define_role" in SYSTEM_PROMPT
        assert "link_role_to_player" in SYSTEM_PROMPT

    def test_system_prompt_contains_link_pattern(self):
        """System prompt should describe the link pattern."""
        assert "Blue" in SYSTEM_PROMPT and "Green" in SYSTEM_PROMPT
        assert "describes" in SYSTEM_PROMPT
        assert "plays" in SYSTEM_PROMPT
        assert "participates" in SYSTEM_PROMPT


class TestUserPromptTemplate:
    """Tests for the user prompt template."""

    def test_user_prompt_template_exists(self):
        """User prompt template should be defined."""
        assert USER_PROMPT_TEMPLATE is not None
        assert len(USER_PROMPT_TEMPLATE) > 0

    def test_user_prompt_template_has_placeholder(self):
        """User prompt should have a placeholder for domain content."""
        assert "{domain_content}" in USER_PROMPT_TEMPLATE

    def test_user_prompt_template_formatting(self):
        """User prompt should format correctly with content."""
        test_content = "This is a test domain description."
        formatted = USER_PROMPT_TEMPLATE.format(domain_content=test_content)
        assert test_content in formatted
        assert "{domain_content}" not in formatted

    def test_user_prompt_mentions_prolog(self):
        """User prompt should mention Prolog output."""
        assert "Prolog" in USER_PROMPT_TEMPLATE

    def test_user_prompt_mentions_archetypes(self):
        """User prompt should reference archetype classification."""
        assert "archetype" in USER_PROMPT_TEMPLATE.lower()
