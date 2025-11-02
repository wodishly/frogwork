#version 410

precision highp sampler2D;

in vec2 v_uv;
out vec4 pc_fragColor;

uniform sampler2D u_texture;

void main() {
    pc_fragColor = texture(u_texture, v_uv);
}
