//
// Copyright (C) 2025  Divya Ranjan Pattanaik
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "render-threads.h"
#include <pthread.h>
#include <stdio.h>

void
job_queue_init(JobQueue *queue)
{
	queue->head = queue->tail = NULL;
	pthread_mutex_init(&queue->q_mutex, NULL);
	pthread_cond_init(&queue->q_cond, NULL);
}

void
job_queue_destroy(JobQueue *queue)
{
	pthread_mutex_destroy(&queue->q_mutex);
	pthread_cond_destroy(&queue->q_cond);
}

void
job_queue_push(JobQueue *queue, ThreadJob *job)
{
	job->next = NULL;
	pthread_mutex_lock(&queue->q_mutex);
	if (queue->tail)
		queue->tail->next = job;
	else
		queue->head = job;
	queue->tail = job;
	pthread_cond_signal(&queue->q_cond);
	pthread_mutex_unlock(&queue->q_mutex);
}

ThreadJob *
job_queue_pop(JobQueue *queue)
{
	pthread_mutex_lock(&queue->q_mutex);
	while (!queue->head || queue->head == NULL) // Waiting for work
		pthread_cond_wait(&queue->q_cond, &queue->q_mutex);
	ThreadJob *job = queue->head;
	queue->head = job->next;
	if (!queue->head) // If there's no next job, set NULL
		queue->tail = NULL;
	pthread_mutex_unlock(&queue->q_mutex);
	return job;
}

void *
thread_routine(void *arg)
{
	ThreadPool *pool = (ThreadPool *)arg;

	while (1)
	{
		// Try to become the leader
		pthread_mutex_lock(&pool->leader_mutex);
		while (pool->leader_exists)
		{
			pthread_cond_wait(&pool->leader_cond,
					  &pool->leader_mutex);
		}

		// Become leader
		pool->leader_exists = 1;
		pthread_mutex_unlock(&pool->leader_mutex);

		// Fetch a task
		ThreadJob *job = job_queue_pop(&pool->job_queue);

		if (job == NULL)
		{
			fprintf(stderr, "The job points to NULL!!!\n");
		}

		// Hand off leadership
		pthread_mutex_lock(&pool->leader_mutex);
		pool->leader_exists = 0;
		pthread_cond_signal(&pool->leader_cond);
		pthread_mutex_unlock(&pool->leader_mutex);

		if (!job->func)
		{
			free(job);
			break;
		}

		job->func(job->arg);
	}

	free(arg);
	return NULL;
}

void
threadpool_init(ThreadPool *pool)
{
	pool->leader_exists = 0;
	pthread_mutex_init(&pool->leader_mutex, NULL);
	pthread_cond_init(&pool->leader_cond, NULL);
	job_queue_init(&pool->job_queue);

	for (int i = 0; i < MAX_POOL_SIZE; i++)
		pthread_create(&pool->threads[i], NULL, thread_routine, pool);
}

void
threadpool_shutdown(ThreadPool *pool)
{
	fprintf(stderr,
		"Joining %d threads and shutting down the thread pool...\n",
		MAX_POOL_SIZE);
	for (int i = 0; i < MAX_POOL_SIZE; i++)
		pthread_join(pool->threads[i], NULL);
	fprintf(stderr, "Destroying the thread job queue..\n");
	job_queue_destroy(&pool->job_queue);
	pthread_mutex_destroy(&pool->leader_mutex);
	pthread_cond_destroy(&pool->leader_cond);
}

void
submit_job(void *(*func)(void *arg), void *arg, ThreadPool *pool)
{
	ThreadJob *job = (ThreadJob *)malloc(sizeof(*job));
	job->func = func;
	job->arg = arg;
	job_queue_push(&pool->job_queue, job);
}
