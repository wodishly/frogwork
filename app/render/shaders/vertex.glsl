#version 410

precision highp float;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;

out vec2 v_uv;
uniform mat4 u_model_matrix;
uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

void main() {
        v_uv = uv;
        gl_Position = u_projection_matrix * u_view_matrix * u_model_matrix * position;
}
