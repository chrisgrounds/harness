// Dear ImGui calls the dear-imgui bindings leave out. This file is compiled
// against cbits/imgui/imgui.h, a copy of the header dear-imgui 2.4.1 builds
// Dear ImGui 1.91.9b from, with the package's default defines, so the
// structs agree with the library it links with; harness_layout_matches
// checks that at start-up.

#include "imgui.h"

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
