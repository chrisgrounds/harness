// Dear ImGui calls the dear-imgui bindings leave out. This file is compiled
// against cbits/imgui/imgui.h, a copy of the header dear-imgui 2.4.1 builds
// Dear ImGui 1.91.9b from, with the package's default defines, so the
// structs agree with the library it links with; harness_layout_matches
// checks that at start-up.

#include "imgui.h"
#include <cmath>

namespace
{
// What the composer's callback does in a frame: the text to replace, if
// any, whether it did, and whether Enter submitted.
struct ComposerFrame
{
    const char* replacement;
    bool replaced;
    bool submitted;
};

// Enter reaches the composer as a newline to filter: without Shift it is
// dropped and submits instead, so the composer stays active rather than
// being deactivated as Enter in Dear ImGui's text boxes otherwise is.
// Pasted newlines arrive without the key and are kept. On every frame the
// composer is active, a pending replacement becomes the whole text, with the
// cursor at its end.
int composer_callback(ImGuiInputTextCallbackData* data)
{
    ComposerFrame* frame = static_cast<ComposerFrame*>(data->UserData);
    if (data->EventFlag == ImGuiInputTextFlags_CallbackCharFilter)
    {
        const bool enter = ImGui::IsKeyPressed(ImGuiKey_Enter, false) || ImGui::IsKeyPressed(ImGuiKey_KeypadEnter, false);
        if (data->EventChar == '\n' && enter && !ImGui::GetIO().KeyShift)
        {
            frame->submitted = true;
            return 1;
        }
    }
    else if (data->EventFlag == ImGuiInputTextFlags_CallbackAlways && frame->replacement != nullptr && !frame->replaced)
    {
        data->DeleteChars(0, data->BufTextLen);
        data->InsertChars(0, frame->replacement);
        data->CursorPos = data->SelectionStart = data->SelectionEnd = data->BufTextLen;
        frame->replaced = true;
    }
    return 0;
}
}

extern "C" {

int harness_layout_matches(void)
{
    return IMGUI_CHECKVERSION() ? 1 : 0;
}

// Fonts are rasterised at the display's pixel density and drawn at this
// scale, so text is sharp on high-density displays. No imgui.ini is kept.
void harness_configure_io(float font_scale)
{
    ImGuiIO& io = ImGui::GetIO();
    io.IniFilename = nullptr;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
    io.FontGlobalScale = font_scale;
}

void harness_set_colour(int colour, float r, float g, float b, float a)
{
    ImGui::GetStyle().Colors[colour] = ImVec4(r, g, b, a);
}

void harness_set_metrics(float window_padding, float frame_padding_x, float frame_padding_y,
                         float item_spacing_x, float item_spacing_y, float rounding, float scrollbar_size)
{
    ImGuiStyle& style = ImGui::GetStyle();
    style.WindowPadding = ImVec2(window_padding, window_padding);
    style.FramePadding = ImVec2(frame_padding_x, frame_padding_y);
    style.ItemSpacing = ImVec2(item_spacing_x, item_spacing_y);
    style.WindowRounding = rounding;
    style.ChildRounding = rounding;
    style.PopupRounding = rounding;
    style.FrameRounding = rounding * 0.75f;
    style.GrabRounding = rounding * 0.75f;
    style.ScrollbarRounding = rounding;
    style.ScrollbarSize = scrollbar_size;
    style.WindowBorderSize = 0.0f;
    style.ChildBorderSize = 1.0f;
    style.FrameBorderSize = 1.0f;
    style.PopupBorderSize = 1.0f;
}

// Sets a style value of one float, such as the opacity or a rounding,
// until the matching PopStyleVar. The bindings only push two.
void harness_push_style_float(int var, float value) { ImGui::PushStyleVar(var, value); }

// A colour for draw lists, faded with the items around it.
unsigned int harness_colour(float r, float g, float b, float a) { return ImGui::GetColorU32(ImVec4(r, g, b, a)); }

void harness_hand_cursor(void) { ImGui::SetMouseCursor(ImGuiMouseCursor_Hand); }

float harness_scroll_y(void) { return ImGui::GetScrollY(); }
float harness_scroll_max_y(void) { return ImGui::GetScrollMaxY(); }
void harness_set_scroll_y(float y) { ImGui::SetScrollY(y); }
void harness_set_scroll_here_y(float ratio) { ImGui::SetScrollHereY(ratio); }

// Whether the key chord was pressed this frame, whichever item owns the
// keys, so shortcuts work while the composer is active.
int harness_chord_pressed(int chord)
{
    return ImGui::IsKeyChordPressed(static_cast<ImGuiKeyChord>(chord)) ? 1 : 0;
}

// Takes the key chord from every other route this frame, including the
// active item, and says whether it was pressed. The composer would
// otherwise revert its text on Escape.
int harness_take_chord(int chord)
{
    return ImGui::Shortcut(static_cast<ImGuiKeyChord>(chord), ImGuiInputFlags_RouteGlobal | ImGuiInputFlags_RouteOverActive) ? 1 : 0;
}

// A multiline text box over buf in which Enter submits and Shift-Enter
// inserts a newline, showing hint while it is empty. With focus it takes
// the keyboard. A replacement, when not null, becomes the text, with the
// cursor at the end, once the box is active; *replaced says whether that
// happened this frame. Returns whether Enter submitted.
int harness_composer(const char* label, const char* hint, char* buf, int buf_size, float width, float height,
                     int focus, const char* replacement, int* replaced)
{
    const ImGuiInputTextFlags flags = ImGuiInputTextFlags_CallbackAlways | ImGuiInputTextFlags_CallbackCharFilter;
    if (focus)
        ImGui::SetKeyboardFocusHere();
    ComposerFrame frame = {replacement, false, false};
    ImGui::InputTextMultiline(label, buf, static_cast<size_t>(buf_size), ImVec2(width, height), flags, composer_callback, &frame);
    if (buf[0] == '\0' && hint != nullptr)
    {
        const ImGuiStyle& style = ImGui::GetStyle();
        const ImVec2 corner = ImGui::GetItemRectMin();
        ImGui::GetWindowDrawList()->AddText(ImVec2(corner.x + style.FramePadding.x, corner.y + style.FramePadding.y),
                                            ImGui::GetColorU32(ImGuiCol_TextDisabled), hint);
    }
    *replaced = frame.replaced ? 1 : 0;
    return frame.submitted ? 1 : 0;
}

}

// Small navigation controls drawn as strokes, independent of installed fonts.
extern "C" int harness_icon_button(const char* id, const char* tooltip, int kind)
{
    const float size = 34.0f;
    ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0, 0, 0, 0));
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);
    bool pressed = ImGui::Button(id, ImVec2(size, size));
    ImGui::PopStyleVar();
    ImGui::PopStyleColor();
    const ImVec2 a = ImGui::GetItemRectMin();
    const ImVec2 c(a.x + size / 2, a.y + size / 2);
    ImDrawList* draw = ImGui::GetWindowDrawList();
    const ImU32 ink = ImGui::GetColorU32(ImGuiCol_Text);
    const float scale = 0.70f;
    const float weight = 1.3f;
    auto point = [&](float x, float y) { return ImVec2(c.x + x * scale, c.y + y * scale); };
    if (kind == 0) {
        // Keep a generous hit target around the smaller settings cog.
        for (int i = 0; i < 48; ++i) {
            const float angle = float(i) * 6.2831853f / 48;
            const float r = (i % 6 == 1 || i % 6 == 2 || i % 6 == 3) ? 9.0f : 6.8f;
            draw->PathLineTo(point(cosf(angle) * r, sinf(angle) * r));
        }
        draw->PathStroke(ink, ImDrawFlags_Closed, weight);
        draw->AddCircle(c, 3.0f * scale, ink, 0, weight);
    } else {
        draw->PathLineTo(point(1, -7));
        draw->PathLineTo(point(-7, -7));
        draw->PathLineTo(point(-7, 7));
        draw->PathLineTo(point(7, 7));
        draw->PathLineTo(point(7, 1));
        draw->PathStroke(ink, ImDrawFlags_None, weight);
        draw->PathLineTo(point(-2, 1));
        draw->PathLineTo(point(6, -7));
        draw->PathLineTo(point(9, -4));
        draw->PathLineTo(point(1, 4));
        draw->PathLineTo(point(-3, 5));
        draw->PathStroke(ink, ImDrawFlags_Closed, weight);
    }
    if (ImGui::IsItemHovered()) {
        ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
        ImGui::SetTooltip("%s", tooltip);
    }
    return pressed ? 1 : 0;
}

extern "C" int harness_splitter(float* width, float minimum, float maximum, float height)
{
    ImGui::InvisibleButton("##sidebar-resize", ImVec2(8, height));
    bool active = ImGui::IsItemActive();
    bool hovered = ImGui::IsItemHovered();
    if (active || hovered) ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeEW);
    if (!active) return 0;
    float next = *width + ImGui::GetIO().MouseDelta.x;
    if (next < minimum) next = minimum;
    if (next > maximum) next = maximum;
    bool changed = next != *width;
    *width = next;
    return changed ? 1 : 0;
}

extern "C" int harness_modal(const char* title, int* open)
{
    bool shown = *open != 0;
    bool visible = ImGui::BeginPopupModal(title, &shown, ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoCollapse);
    *open = shown ? 1 : 0;
    return visible ? 1 : 0;
}

extern "C" int harness_sized_button(const char* label, float width, float height)
{
    return ImGui::Button(label, ImVec2(width, height)) ? 1 : 0;
}

extern "C" int harness_model_combo(const char* id, const char* preview, float width)
{
    ImGui::SetNextItemWidth(width);
    ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0, 0, 0, 0));
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);
    bool opened = ImGui::BeginCombo(id, preview, ImGuiComboFlags_NoArrowButton | ImGuiComboFlags_HeightLarge);
    ImGui::PopStyleVar();
    ImGui::PopStyleColor();
    // Draw the affordance in the preview, without a separate arrow button.
    if (!opened) {
        ImVec2 a = ImGui::GetItemRectMax();
        float cy = (ImGui::GetItemRectMin().y + a.y) / 2;
        ImDrawList* draw = ImGui::GetWindowDrawList();
        ImU32 ink = ImGui::GetColorU32(ImGuiCol_TextDisabled);
        draw->AddLine(ImVec2(a.x - 13, cy - 2), ImVec2(a.x - 9, cy + 2), ink, 1.3f);
        draw->AddLine(ImVec2(a.x - 9, cy + 2), ImVec2(a.x - 5, cy - 2), ink, 1.3f);
    }
    return opened ? 1 : 0;
}
