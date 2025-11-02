#version 410

in vec2 v_uv;
out vec4 fColor;

void main() {
    fColor = vec4(v_uv, v_uv.x+v_uv.y, 1.0);
}
