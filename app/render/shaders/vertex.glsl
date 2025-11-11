#version 410

precision highp float;

layout(location = 0) in vec4 position;
layout(location = 1) in vec2 uv;
layout(location = 2) in vec3 normal;

out vec2 v_uv;
out vec3 v_normal;
uniform mat4 u_model_matrix;
uniform mat4 u_projection_matrix;
uniform mat4 u_view_matrix;

void main() {
    v_uv = uv;

    mat4 modelview_matrix = u_view_matrix * u_model_matrix;
    mat3 normal_matrix = transpose(inverse(mat3(modelview_matrix)));
    v_normal = normal_matrix * normal;

    gl_Position = u_projection_matrix * modelview_matrix * position;
}
